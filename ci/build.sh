#!/usr/bin/env bash
#
# lib-ai's build (ISS-2173). READ devops/templates/ci/build.sh AND
# devops/docs/ci.md FIRST: the four load-bearing properties — landing this file
# IS the enrolment, `set -euo pipefail` is not style, the fleet posts the `ci`
# context, and `# ci-needs:` drives the preflight — are explained there and are
# unchanged here.
#
# WHAT A GREEN HERE VOUCHES FOR: this library's own sources compile under
# `-Werror` with the full `-Wunused` set and its own ScalaTest suite passes, on
# the exact commit about
# to be squashed.
#
# WHAT IT DOES NOT VOUCH FOR, and this is the important half: THAT ANY CONSUMER
# STILL COMPILES. Nothing in the fleet builds `platform` or `acumen` against a
# lib PR's tree. A consumer resolves a RELEASED version out of Maven Central, and
# no released version exists until a human runs `dev release lib` — which prompts
# and shells out to GPG, so a merge here publishes nothing. So a PR that retypes
# a client method is green here and breaks platform at the moment somebody bumps
# the pin. That break surfaces in the CONSUMER's own version-bump PR, verified by
# the consumer's own `ci`, which is where `dev release lib` and
# `Agent::LibraryPin` already put it. Enrolling this repo narrows nothing about
# that and is not meant to.
#
# AND IT VOUCHES FOR NOTHING ABOUT api.anthropic.com. This repo is service-shaped
# — `spec/`, `app/`, `test/`, a `.api` config — but every one of its Claude tests
# runs against captured wire fixtures and a `TestClaudeClient`, with no network
# call and no credential. That is the property that makes it verifiable by an
# unattended job at all, and it means a green here says the client still parses
# the bodies Anthropic sent on the day they were captured, not that Anthropic
# still sends them.
#
# THE GENERATED CLIENT IS COMMITTED (`app/scala/com/bryzek/ai/generated`), so
# this build does NOT run `api` and needs no apibuilder credential. A spec change
# that was not regenerated is caught here as a compile error like any other, and
# `codegen-sync` is what notices drift on `main`.
#
# NO DATABASE BLOCK, and that is the whole difference from
# devops/templates/ci/build-scala.sh. That template exists for platform and
# acumen, which reach a Postgres through `CONF_DB_DEV_URL`; this suite opens no
# connection to anything. Copying the block would start a container, hold a host
# port from the range `dev db session` allocates out of, and depend on the
# DigitalOcean registry credential — three ways to go red that have nothing to do
# with the code under test, and the session-database port range is the single
# biggest producer of false red in the fleet (ISS-2178).
#
# FORMATTING IS REPORTED, NEVER ENFORCED (ISS-19427). A build fails only for a
# functional problem, and an unformatted file behaves exactly as its formatted
# copy does. `scalafmtOnCompile := true` formats `app/` and `test/`, and
# `scripts/check-format` (a copy of devops templates/ci/check-format, which runs
# `dev fmt report`: the native scalafmt CLI over the sources AND `build.sbt`,
# which nothing else formats) prints every unformatted file and exits 0. The
# hygiene sweep runs it over `main` and fixes what it finds.
#
# `# ci-needs: heap:4G` — MEASURED, not copied (ISS-1171's method, run
# 2026-08-11). Cold checkout, `SBT_OPTS="-Xmx4G -Xss4M"`, process-tree RSS
# sampled with `/bin/ps -axo pid=,ppid=,rss=`: peak 1.7 GB across the whole tree
# INCLUDING the forked test JVM, exit 0, 113 tests passed. 4G is
# `Agent::Heap::MIN_GB` — the least any runner hands a build, so nothing below it
# is worth measuring and this excludes no machine. The token is here rather than
# omitted because an omission is indistinguishable from nobody having looked.
#
# THIS IS THE ONE LIB THAT FORKS ITS TEST JVM (`Test / fork := true`, from the
# Play plugin), and a forked JVM inherits none of `SBT_OPTS` — left unset its max
# heap is a QUARTER OF PHYSICAL RAM, which is 16G on the 64G laptop and 6G on the
# 24G mini. So the same suite would be allowed different memory on the two
# runners and the measurement above would be a fact about the machine rather than
# about the suite. `build.sbt` pins `Test / javaOptions += "-Xmx4g"` for that
# reason, exactly as platform and acumen do.
#
# ci-needs: heap:4G
#
# TEST HEALTH READS THE NEXT TWO LINES (ISS-19887, design DES-18577 "Every repository
# with CI"). The first is where sbt leaves a JUnit report per suite, which is what the
# fleet writes the test ledger from: `TEST-<suite>.xml` is the build's run of a suite,
# `FIRST-<suite>.xml` the first run of one the build reran. The second says this build
# runs with the lists platform holds: whatever starts it fetches them first and exports
# the file as `CI_TEST_LISTS`. ci/test-health.sh and the three project/Ci*.scala files
# are the runner half, copies of devops templates/sbt-test-health held to it below.
#
# ci-test-reports: target/test-reports/*.xml
# ci-test-lists: required
set -euo pipefail

echo "building ${CI_REPO:-?} @ ${CI_SHA:-?} (${CI_EVENT:-?}, clean=${CI_CLEAN_BUILD:-?})"

# A GENERATED CLIENT BUILT FROM A SPEC NOBODY MERGED FAILS THIS BUILD (ISS-10269).
# `dev api` resolves a sibling spec-owning clone under `~/code/ai/<feature>/` ahead of
# anything merged, which is what makes a cross-repo change reviewable on one branch —
# and the consumer half can then merge on its own while the spec it was generated from
# is never pushed. trips shipped a reservation kind that way, and every booking of it
# was refused as unrecognized (ISS-10261). Neither guard around it sees that: the push
# guard asks whether a codegen run happened in this checkout, and one did, and
# `dev codegen lint-consumers` asks about hand-written calls.
#
# IT JUDGES ONLY WHAT THE BRANCH ADDS — a contract symbol its copy of a generated file
# declares that neither the fork point's copy nor a regeneration from the producers'
# main mentions. So a producer spec the `codegen-sync` sweep has not carried over yet,
# and a generator release, say nothing here; neither is anything the branch added. A
# branch that changes no generated file pays one fetch and stops, so an ordinary pull
# request costs almost nothing.
#
# EXIT 2 IS THE BOX, NEVER THE BRANCH — no fork point, a `dev repo` mirror or an
# apibuilder that did not answer — and is mapped to 75 so the lane re-enqueues the head
# instead of parking the pull request on a red nobody can act on. 1 is the branch.
#
# THE LEGITIMATE REFUSAL is the consumer half of an OPEN cross-repo change:
# `DEV_REPO_PENDING=<producer>#<n> git push origin <branch>` measures this client
# against the producer's pull request head and records the `pending:` label. The fleet's
# own run sets nothing, so this pull request stays red until that one merges, which is
# the order the two have to land in.
rc=0; dev codegen landed || rc=$?
case "$rc" in
  0) echo "dev codegen landed: every contract symbol this branch adds comes out of the producers' merged specs" ;;
  2) echo "ci/build.sh: dev codegen landed could not measure this head on this box" >&2; exit 75 ;;
  *) exit "$rc" ;;
esac

# THE SECURITY PINS ARE OWNED BY devops, and this is what holds the copies here to them.
# project/BryzekPins.scala declares the Jackson 2, Jackson 3 and logback versions every lib-*
# build resolves, and the pin specs assert them as behaviour of the resolved jars. All of them are
# byte-identical copies of devops's templates/scala-libs, read off devops's main with
# `dev repo cat` (no fetch of our own, ISS-2232), so an advisory bump is one edit there and a copy
# that is not taken fails here rather than drifting into a per-repo floor. A read that fails
# leaves the diff comparing against nothing, which fails too. ISS-15715
#
# THE TEST HEALTH RUNNER IS OWNED THE SAME WAY (ISS-19887): templates/sbt-test-health holds the
# lists filter, the rerun and the shell half every sbt repository runs its suite through.
synced=(
  "scala-libs/project/BryzekPins.scala project/BryzekPins.scala"
  "scala-libs/test/JacksonPinSpec.scala test/scala/com/bryzek/pins/JacksonPinSpec.scala"
  "scala-libs/test/LogbackPinSpec.scala test/scala/com/bryzek/pins/LogbackPinSpec.scala"
  "sbt-test-health/project/CiTestLists.scala project/CiTestLists.scala"
  "sbt-test-health/project/CiTestListsPlugin.scala project/CiTestListsPlugin.scala"
  "sbt-test-health/project/CiTestRerunPlugin.scala project/CiTestRerunPlugin.scala"
  "sbt-test-health/ci/test-health.sh ci/test-health.sh"
)
for pair in "${synced[@]}"; do
  read -r template copy <<<"$pair"
  if ! diff -u <(dev repo cat devops "templates/$template") "$copy"; then
    echo "ci/build.sh: sync it -- dev repo cat devops templates/$template > $copy" >&2
    exit 1
  fi
done

# ci-advisory: scripts/check-format
scripts/check-format || echo "check-format: could not measure this tree — advisory, nothing fails on it"

# THE SUITE, WITH TEST HEALTH (ISS-19887). `ci_test_health_sbt` prints the `ci-lists:` line for
# the lists `CI_TEST_LISTS` names (exiting 75 on a file that is not one, or on `CI_SLOW_SPECS=1`
# with none), and `ciTest` runs every spec the lists leave in -- the slow ones alone under
# `CI_SLOW_SPECS=1` -- then reruns each spec that failed once, printing `ci-flake:` per spec that
# passed the second time and `ci-rerun: done`. A spec that fails twice fails the build.
source ci/test-health.sh
ci_test_health_sbt
sbt -Dsbt.log.noformat=true "${ci_sbt_test_commands[@]}"
