import sbt._

/** The security pins every lib-* build resolves against: Jackson 2, Jackson 3 and logback.
  *
  * THIS FILE IS OWNED BY devops (templates/scala-libs/project/BryzekPins.scala) AND COPIED
  * VERBATIM into each lib's `project/`. Each lib's `ci/build.sh` diffs its copy against devops's
  * `main` and fails on any difference, so an advisory bump is one edit there plus one copy per
  * repo, and a copy that is not taken fails that repo's `ci` rather than drifting. Edit it there,
  * never here:
  *
  * {{{
  * dev repo cat devops templates/scala-libs/project/BryzekPins.scala > project/BryzekPins.scala
  * }}}
  *
  * The values only DECLARE the pins. Each build.sbt chooses which apply to it -- Jackson 3 is only
  * on the classpath of the libs that resolve logstash-logback-encoder 9 -- and how: as
  * `ThisBuild / dependencyOverrides`, which governs THAT build's resolution only (sbt writes no
  * `dependencyOverrides` into the published POM, so it imposes no floor on a consumer), or as a
  * declared dependency, which does reach consumers. The pin specs in templates/scala-libs/test
  * assert each pin as behaviour of the resolved jars, because every way a pin can be wrong
  * resolves cleanly and says nothing.
  */
object BryzekPins {

  // Every Jackson 2 artifact in a build resolves to one version.
  //
  // Jackson's own compatibility rule is that a release train moves together: the datatype and
  // dataformat modules compile against databind's internal serializer/deserializer SPI, and
  // jackson-module-scala additionally asserts its databind version at runtime and refuses to
  // register outside its own minor line. Only that last one fails loudly; a datatype module left
  // behind on an older line links fine and throws AbstractMethodError or NoSuchMethodError on
  // whichever serializer path first touches a changed SPI method. `JacksonPinSpec` asserts the
  // pair registers, so a partial bump fails by name rather than opaquely in a consumer.
  //
  // Drift is the default rather than an accident. play-json and pekko-serialization-jackson
  // contribute the whole family transitively at one version, so pinning a single coordinate wins
  // the conflict only for that artifact and the ones it depends on, leaving cbor/jdk8/jsr310/
  // parameter-names behind. Overriding the whole family is what makes one version true of all of
  // them.
  //
  // The floor is a security one and six advisories set it. jackson-core below 2.15.0 has no
  // nesting-depth limit and throws StackOverflowError on deeply nested input rather than rejecting
  // it (GHSA-h46c-h94j-95f3), and that is the version play-json resolves. jackson-databind below
  // 2.18.8 -- and again on 2.19.0 through 2.21.3 -- validates a type id carrying generics by the
  // substring before the `<` and then resolves the type arguments out of the rest of it without
  // ever offering them to the PolymorphicTypeValidator, so an allow-list naming one safe container
  // admits any type smuggled into that container's parameter position (GHSA-j3rv-43j4-c7qm).
  // Databind over that same range also answers `allowIfSubTypeIsArray` on `clazz.isArray()` alone
  // and never validates the array's component type, so a denied class named as the element of an
  // array is admitted and instantiated with no further check (GHSA-rmj7-2vxq-3g9f). jackson-core
  // over that same range applies maxNumberLength to the digits within each chunk fed to the
  // non-blocking parser rather than to the number accumulated across feeds, so a number split
  // across `feedInput` calls is not bounded at all (GHSA-r7wm-3cxj-wff9).
  //
  // jackson-databind carries two more, fixed on each release line separately: below 2.18.11, on
  // 2.19.0 through 2.21.6, and on 2.22.0 through 2.22.2 it completes forward object-id references
  // in time quadratic in their number (GHSA-cxp5-3px4-pw24), and retains every unknown raw type id
  // it is handed (GHSA-wv8q-qhhj-9h54).
  //
  // Those two are the binding ones: the lowest this pin may state is 2.18.11, anything chosen on
  // the 2.19-2.21 lines must be 2.21.7 or above, and anything on the 2.22 line must be 2.22.3 or
  // above. 2.22.3 is the head of the Jackson 2 line and the version platform and acumen pin, so a
  // consumer that pins too resolves one Jackson rather than two.
  //
  // jackson-annotations publishes no patch versions on its 2.20+ lines (maven-metadata.xml runs
  // 2.19.4, 2.20, 2.21, 2.22), so it carries its own version and a patch number there is a 404
  // that fails the whole resolution.
  val jacksonVersion = "2.22.3"
  val jacksonAnnotationsVersion = "2.22"

  val jackson2: Seq[ModuleID] = Seq(
    "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,
    "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
    "com.fasterxml.jackson.core" % "jackson-annotations" % jacksonAnnotationsVersion,
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor" % jacksonVersion,
    "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % jacksonVersion,
    "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310" % jacksonVersion,
    "com.fasterxml.jackson.module" % "jackson-module-parameter-names" % jacksonVersion,
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % jacksonVersion,
  )

  // Jackson 3 -- the `tools.jackson` coordinates -- resolves to one version too. OPT-IN: only a
  // build whose classpath carries it states it, which is a build resolving
  // net.logstash.logback:logstash-logback-encoder 9, directly or through lib-util. It is a separate
  // family from the `com.fasterxml.jackson` one above rather than a newer release of it, and the
  // two coexist: the packages differ, so neither shadows the other and conflict resolution never
  // puts them in the same bucket. Jackson 3's databind still depends on the 2.x
  // `com.fasterxml.jackson.core:jackson-annotations` -- there is no `tools.jackson.core`
  // annotations artifact -- and the 3.1 line asks for 2.21, which the annotations pin above
  // already satisfies.
  //
  // The encoder moved its JSON encoding to Jackson 3 and declares tools.jackson.core:jackson-databind
  // 3.0.1 at compile scope; databind brings tools.jackson.core:jackson-core with it. Those two are
  // the whole of Jackson 3 on the classpath: the encoder's cbor, smile and yaml dataformat
  // dependencies are declared `optional` and resolve nowhere. Enabling one of the decorators that
  // needs one means declaring that artifact at this version, not inheriting whatever the encoder's
  // POM names. A build that pins nothing resolves the encoder's own 3.0.1, which twelve advisories
  // are open against -- and an upstream lib's override does not help, because it is not in the POM.
  //
  // The floor is a security one. jackson-core below 3.1.4 applies maxNumberLength to the digits
  // within each chunk fed to the non-blocking parser rather than to the number accumulated across
  // feeds (GHSA-r7wm-3cxj-wff9), and below 3.1.1 bypasses the document length limit
  // (GHSA-2m67-wjpj-xhg9). jackson-databind below 3.1.4 admits a denied type through a generic
  // argument (GHSA-j3rv-43j4-c7qm) or as an array's component (GHSA-rmj7-2vxq-3g9f), and below
  // 3.1.5 replays a `@JsonUnwrapped` property's buffered JSON without asking whether that property
  // is visible in the active view, so a property a write path excluded with `@JsonView` is
  // populated from the document anyway (GHSA-5gvw-p9qm-jgwh).
  //
  // Databind below 3.1.6 carries three more: `DefaultBaseTypeLimitingValidator` leaves
  // `Comparable` off the base types it refuses, so polymorphic typing declared against one admits
  // any subtype (GHSA-gx83-3vf8-gh7j); `Duration` and `XMLGregorianCalendar` deserialization parse
  // a number of unbounded length (GHSA-q4xh-88c3-wmh7); and `Path` deserialization resolves
  // whatever `FileSystemProvider` scheme the document names rather than an allowlist
  // (GHSA-wjgm-6hv5-3cvf). Below 3.1.7 it carries two more again: it completes forward object-id
  // references in time quadratic in their number (GHSA-cxp5-3px4-pw24), and retains every unknown
  // raw type id it is handed (GHSA-wv8q-qhhj-9h54).
  //
  // Databind's is the binding one, so 3.1.7 is the lowest this pin may state, and it is the
  // version platform and acumen pin. The 3.2 line clears all of them from 3.2.3 and is deliberately
  // not chosen: it is a further minor above what the encoder was compiled against, and the encoder
  // reaches Jackson only through internal SPI that a minor line is free to move. `Jackson3PinSpec`
  // asserts two of those limits behaviourally, so a pin that slips below the floor fails there by
  // name.
  val jackson3Version = "3.1.7"

  val jackson3: Seq[ModuleID] = Seq(
    "tools.jackson.core" % "jackson-databind" % jackson3Version,
    "tools.jackson.core" % "jackson-core" % jackson3Version,
  )

  // logback moves as a PAIR, and the version is a security floor that FOUR advisories set.
  //
  // GHSA-25qh-j22f-pwp8: logback-core evaluates a conditional configuration element
  // (`<if>`/`<then>`, compiled by Janino) out of the configuration file it was handed, so whoever
  // can write that file or set the environment variable naming it chooses code the JVM then runs.
  // Fixed in 1.5.19.
  //
  // GHSA-qqpg-mvqg-649v: logback-core below 1.5.25 resolves an `<appender-ref>` out of the appender
  // bag without ever asking whether the configuration DECLARED an appender of that name. It is an
  // ACE against configuration processing too, but the part that shows on a healthy build is
  // quieter: a reference to a name that was never declared leaves the referring logger with NO
  // appenders at all, the declared ones beside it included, and records nothing about it. 1.5.25
  // adds the declaration check, so an undeclared reference is warned about and skipped and the
  // declared appenders beside it are still attached.
  //
  // GHSA-p47f-322f-whfh: through 1.5.32, logback-core's `HardenedObjectInputStream` -- the
  // deserializer behind `SimpleSocketServer` and `SimpleSSLSocketServer` -- decided what a
  // socket-delivered logging event may instantiate by PREFIX: a class name beginning `java.lang`
  // or `java.util` was admitted whatever class it actually named. From 1.5.33 the same decision is
  // an equality test against sixteen named classes.
  //
  // GHSA-jhq6-gfmj-v8fx: that check bounds what a stream may NAME, and through 1.5.33 nothing
  // bounded what the stream may have the JVM SYNTHESISE. `HardenedObjectInputStream` left
  // `resolveProxyClass` to `ObjectInputStream`, which defines a proxy class for whatever interface
  // names the stream carries before any of logback's checks can run. A caller-supplied whitelist
  // naming `java.lang.reflect.Proxy` and the handler's class then lets the stream choose the
  // interfaces AND the `InvocationHandler` behind them. 1.5.34 refuses proxies unconditionally, so
  // 1.5.34 is the lowest this pin may state.
  //
  // 1.6.5 rather than that floor: it clears all four, carries no open advisory of its own, and is
  // the line Play 3.0.12's play-logback resolves (1.6.4), so a lib that puts Play on its classpath
  // and one that only tests against play-test resolve the same logback, and one floor holds across
  // every lib-* build rather than one per repo.
  //
  // BOTH COORDINATES, AT ONE VERSION, for three reasons that point the same way. logback publishes
  // classic and core as one train: classic subclasses core's appender, model and joran types, and
  // its OSGi manifest imports `ch.qos.logback.core` at `[1.5,2)` rather than at a floor, so moving
  // one alone resolves cleanly and breaks where a version conflict is hardest to read -- the first
  // time a logger is configured, as a NoSuchMethodError from inside logback. The declaration check
  // spans them as well: its analyser lives in logback-core but logback-classic registers it, so
  // logback-core alone at 1.5.25+ leaves the guard registered by nobody and EVERY appender-ref is
  // skipped. And the hardened-stream fix changed `HardenedObjectInputStream`'s constructors to take
  // a `Context`, which logback-classic 1.5.32's `HardenedLoggingEventInputStream` does not pass, so
  // bumping core alone throws NoSuchMethodError at class initialization. `LogbackPinSpec` asserts
  // every one of these as behaviour of the resolved pair.
  //
  // How a build applies it is its own decision, stated in its build.sbt: a lib that never loads
  // logback outside its suite overrides it, so no logback edge reaches its POM; a lib that puts
  // logback on its consumers' classpath (through Play) DECLARES it, so the floor reaches them.
  val logbackVersion = "1.6.5"

  val logback: Seq[ModuleID] = Seq(
    "ch.qos.logback" % "logback-classic" % logbackVersion,
    "ch.qos.logback" % "logback-core" % logbackVersion,
  )
}
