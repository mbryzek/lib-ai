# Test Health for an sbt build (ISS-19887, design DES-18577 "Every repository with CI").
#
# SOURCED by ci/build.sh, never run: it defines functions and runs nothing. This file is a copy of
# devops templates/sbt-test-health/ci/test-health.sh, held to it by the repository's ci/build.sh
# with the three project/ files beside it; edit it there.
#
# The four lines of the contract, and where each one lives:
#
#   1. reports  ci/build.sh declares `# ci-test-reports: <glob>`, where sbt leaves a JUnit report
#               per suite (`<subproject>/target/test-reports/TEST-<suite>.xml`).
#   2. lists    ci/build.sh declares `# ci-test-lists: required`. `ci_test_health_sbt` below reads
#               the file `CI_TEST_LISTS` names and prints the `ci-lists:` line; sbt reads the same
#               file (project/CiTestListsPlugin.scala) and leaves out every quarantined and slow spec.
#   3. slow     `CI_SLOW_SPECS=1` inverts that filter to the slow specs alone.
#   4. rerun    `ciTest` (project/CiTestRerunPlugin.scala) reruns every failed spec once and prints
#               `ci-flake:` and `ci-rerun: done`.
#
# Use:
#
#   source ci/test-health.sh
#   ci_test_health_sbt
#   sbt -Dsbt.log.noformat=true "${ci_sbt_test_commands[@]}"

# devops' `Agent::Ci::INFRA_EXIT_CODE`: nothing this build printed is a fact about the tree.
CI_TEST_HEALTH_INFRA_EXIT=75

# `CI_TEST_LISTS` names a file: its first line is the lists version and every line after it is
# `quarantined <spec>`, `slow <spec>` or `proof <spec>`. A file that is not exactly that format is
# refused whole: a line this does not understand is a spec the build would run while the lists say
# it must not. `awk` runs its END block after an `exit`, which is what the `bad` flag is for.
ci_lists_summary() {               # ci_lists_summary <file>
  awk '
    function refuse(why) { printf "line %d: %s\n", NR, why >"/dev/stderr"; bad = 1; exit 2 }
    { sub(/\r$/, "") }
    NR == 1 { if ($0 ~ /^(version )?[0-9]+$/) { version = $NF; next } refuse("not a lists version") }
    /^[ \t]*$/ { next }
    /^quarantined [^ \t]+$/ { quarantined++; next }
    /^slow [^ \t]+$/ { slow++; next }
    /^proof [^ \t]+$/ { next }
    { refuse("neither quarantined, slow nor proof") }
    END {
      if (bad) exit 2
      if (NR == 0) { print "the file is empty" >"/dev/stderr"; exit 2 }
      printf "ci-lists: version %s quarantined %d slow %d\n", version, quarantined, slow
    }
  ' "$1"
}

# Prints the `ci-lists:` line and `ci-scope:`, and sets `ci_sbt_test_commands` to what sbt runs.
#
# UNSET LEAVES NOTHING OUT. That is a session's working tree, and it is how the session fixing a
# quarantined spec runs it. A lists file that is not the format, and `CI_SLOW_SPECS=1` with no lists
# (there is no slow spec to run, and a green would claim there was), are the box and not the branch:
# the build exits 75 with nothing measured.
#
# `ciListsUnknown` rides ahead of `ciTest` in the same invocation when there are lists and this is
# not the slow run: it prints `ci-lists: unknown <spec>` for a listed spec no subproject defines,
# and never fails.
ci_test_health_sbt() {
  local slow=false
  [ "${CI_SLOW_SPECS:-}" != "1" ] || slow=true
  ci_sbt_test_commands=(ciTest)
  if [ -n "${CI_TEST_LISTS:-}" ]; then
    if ! ci_lists_summary "$CI_TEST_LISTS"; then
      echo "ci/build.sh: INFRASTRUCTURE FAULT — CI_TEST_LISTS=$CI_TEST_LISTS is not a lists file, so this build cannot know which specs to leave out; nothing about the tree was measured" >&2
      exit "$CI_TEST_HEALTH_INFRA_EXIT"
    fi
    [ "$slow" = "true" ] || ci_sbt_test_commands=(ciListsUnknown ciTest)
  elif [ "$slow" = "true" ]; then
    echo "ci/build.sh: INFRASTRUCTURE FAULT — CI_SLOW_SPECS=1 runs only the slow specs and CI_TEST_LISTS names no lists; nothing about the tree was measured" >&2
    exit "$CI_TEST_HEALTH_INFRA_EXIT"
  fi
  # The slow run is not `full`: a repository's slow ceiling is a share of its full builds' test
  # phase, and a run of the slow specs alone is not one of those.
  if [ "$slow" = "true" ]; then
    echo "ci-scope: slow-specs"
  else
    echo "ci-scope: full every subproject"
  fi
}
