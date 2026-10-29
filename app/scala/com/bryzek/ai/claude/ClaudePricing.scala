package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeModel, ClaudeUsage, KnownClaudeServiceTier}

/** Per-million-token USD list pricing for the Claude API, by wire model id (`ClaudeModel.toString`,
  * e.g. `"claude-opus-5"`). Hardcoded rather than a DB tunable -- these change only when Anthropic
  * publishes a new price list, at which point this file needs a one-line-per-model edit anyway;
  * see https://platform.claude.com/docs/en/pricing (or the pricing table in the claude-api skill)
  * for the current numbers when updating.
  *
  * The id priced here is the one the API ECHOES BACK on the response, not the one we sent, and those
  * differ: an alias resolves to the dated SNAPSHOT it currently points at, so a request for
  * `claude-haiku-4-5` comes back as `claude-haiku-4-5-20251001`. A snapshot is the same model at the
  * same price as its alias, so [[normalize]] strips the date rather than this table carrying a literal
  * per snapshot -- otherwise every alias repoint silently reverts pricing to $0 until someone notices.
  */
object ClaudePricing {

  /** What a cache WRITE costs, as a multiple of the model's input rate, per TTL.
    *
    * These are the whole reason `claude.responses` and `claude.usage_daily_rollups` carry the 1-hour
    * write as its own column: one `cache_creation_input_tokens` total priced at 1.25x reads as cheaper
    * than a 1h write actually is, and under-reporting is the direction that hides a longer TTL which
    * has stopped paying for itself (ISS-1942). Public because
    * platform's `core.invariants.ClaudeCacheInvariants` compares the same two multipliers against the uncached
    * rate in SQL, and that comparison must not drift from what this file charges.
    */
  val CacheWrite5mMultiplier: BigDecimal = BigDecimal("1.25")
  val CacheWrite1hMultiplier: BigDecimal = BigDecimal("2")

  /** What a cache READ costs, as a multiple of the input rate, for a model on Anthropic's STANDARD
    * multiplier -- which is every model here but Claude Opus 5.5. The same at either TTL: the TTL
    * decides how long an entry stays readable, not what reading it costs.
    *
    * A standard multiplier is no longer a property of the price list, which is why [[Rate]] carries
    * the read rate outright instead of deriving it here. Claude Opus 5.5 reads at 0.05x its input
    * rate and Claude Fable 5.1 at 0.025x, against 0.1x everywhere else; deriving the read from the
    * input would have priced Opus 5.5's reads at double, silently, in the one token category an
    * agent workload spends most of. This constant stays because it is still the right default for a
    * model being added, and because platform's `core.invariants.ClaudeCacheInvariants` needs the standard
    * value to build its per-model comparison from.
    */
  val CacheReadMultiplier: BigDecimal = BigDecimal("0.1")

  private case class Rate(inputPerM: BigDecimal, outputPerM: BigDecimal, cacheReadPerM: BigDecimal) {
    def cacheWrite5mPerM: BigDecimal = inputPerM * CacheWrite5mMultiplier
    def cacheWrite1hPerM: BigDecimal = inputPerM * CacheWrite1hMultiplier
  }

  private object Rate {

    /** A model on the standard cache-read multiplier -- the common case, spelled so that the
      * exception is the row that looks unusual rather than the rule.
      */
    def standardCacheRead(inputPerM: BigDecimal, outputPerM: BigDecimal): Rate =
      Rate(inputPerM, outputPerM, inputPerM * CacheReadMultiplier)
  }

  private val Rates: Map[String, Rate] = Map(
    // Cache reads at $0.20/MTok are 0.05x this model's input rate, not the 0.1x every other model
    // here charges -- written out rather than derived, which is the whole reason [[Rate]] takes it.
    "claude-opus-5-5" ->
      Rate(inputPerM = BigDecimal(4), outputPerM = BigDecimal(20), cacheReadPerM = BigDecimal("0.20")),
    "claude-opus-5" -> Rate.standardCacheRead(inputPerM = BigDecimal(5), outputPerM = BigDecimal(25)),
    // $2/$10 is the standard price, not an introductory one: the increase to $3/$15 once announced for
    // September 2026 was cancelled (ISS-13608).
    "claude-sonnet-5-5" -> Rate.standardCacheRead(inputPerM = BigDecimal(2), outputPerM = BigDecimal(10)),
    "claude-sonnet-5" -> Rate.standardCacheRead(inputPerM = BigDecimal(2), outputPerM = BigDecimal(10)),
    "claude-haiku-4-5" -> Rate.standardCacheRead(inputPerM = BigDecimal(1), outputPerM = BigDecimal(5)),
    // The rate for a prompt of 100K tokens or fewer. Anthropic charges 0.50/2.50 above that, which no row here
    // models: no caller sends Haiku a prompt that long, and a flat row that under-reports the long tail is named as
    // such rather than averaged. Revisit if a caller ever does.
    "claude-haiku-5-5" -> Rate.standardCacheRead(inputPerM = BigDecimal("0.10"), outputPerM = BigDecimal("0.50")),
    // Cache reads at $0.25/MTok are 0.025x the input rate, against claude-fable-5's standard 0.1x at the same
    // $10/$50 -- the same trap as claude-opus-5-5, one row apart.
    "claude-fable-5-1" ->
      Rate(inputPerM = BigDecimal(10), outputPerM = BigDecimal(50), cacheReadPerM = BigDecimal("0.25")),
    "claude-fable-5" -> Rate.standardCacheRead(inputPerM = BigDecimal(10), outputPerM = BigDecimal(50)),
  )

  /** The cache-read multiplier of every model that is NOT on [[CacheReadMultiplier]], derived from
    * [[Rates]] rather than listed, so a rate corrected above cannot leave a second copy behind.
    *
    * Exists for platform's `core.invariants.ClaudeCacheInvariants`, which compares the cost of caching against
    * not caching in SQL and so needs the read ratio per model rather than the one constant it used
    * while every model shared a ratio. Keyed by the same normalized id [[normalize]] produces.
    */
  val NonStandardCacheReadMultipliers: Map[String, BigDecimal] =
    Rates.collect {
      case (model, rate) if rate.cacheReadPerM != rate.inputPerM * CacheReadMultiplier =>
        model -> (rate.cacheReadPerM / rate.inputPerM)
    }

  private val PerMillion = BigDecimal(1000000)

  /** What the tier that served a request multiplies its cost by, applied to ALL four token types.
    *
    * The Batches API is 50% off every token type, not just input, and -- verified against
    * api.anthropic.com on 2026-08-11 -- a batched response reports RAW, UNDISCOUNTED token counts
    * with `service_tier: "batch"`. So the discount exists nowhere in the row except this multiplier:
    * without it, the moment anything here batches, every batched request is costed at exactly 2x.
    *
    * `Priority` is 1x and that is a known UNDER-report, not an oversight. Anthropic publishes no
    * multiplier for Priority Tier -- it is a capacity commitment sold at a premium -- so the honest
    * options were 1x or an invented number, and an invented number in a cost column is worse than a
    * documented approximation. Nothing here can currently produce one: Priority Tier does not cover
    * Claude Opus 5, Sonnet 5, Fable 5, Fable 5.1 or Mythos 5, which is every model in [[Rates]] but haiku
    * and Claude Opus 5.5, whose Priority Tier status Anthropic has not published. Revisit when a rate is
    * published.
    *
    * `None` -- a row written before the tier was recorded, or a tier this platform does not
    * recognize (see [[ClaudeServiceTiers]]) -- is also 1x, deliberately erring high.
    */
  private def tierMultiplier(serviceTier: Option[KnownClaudeServiceTier]): BigDecimal = {
    serviceTier match {
      case None => BigDecimal(1)
      case Some(KnownClaudeServiceTier.Standard) => BigDecimal(1)
      case Some(KnownClaudeServiceTier.Priority) => BigDecimal(1)
      case Some(KnownClaudeServiceTier.Batch) => BigDecimal("0.5")
    }
  }

  /** Anthropic's dated-snapshot suffix: the alias plus `-YYYYMMDD` (`claude-haiku-4-5-20251001`).
    * Anchored on exactly eight digits at the end so it can only ever strip a date -- a future model
    * whose name merely extends an existing one (`claude-opus-5-1`) is left alone and priced as the
    * unknown model it is, rather than silently inheriting the older model's rate.
    */
  private val DatedSuffix = """^(.*)-\d{8}$""".r

  def normalize(model: String): String = model match {
    case DatedSuffix(alias) => alias
    case _ => model
  }

  /** [[normalize]] as a SQL expression over `column`, for a caller that has to compare a stored model
    * id against [[Rates]]' keys inside a query rather than in Scala.
    *
    * A second spelling of [[DatedSuffix]] and deliberately adjacent to it, because that is the only
    * thing keeping the two in step -- there is no way to run a Scala regex inside Postgres, and the
    * alternative (a caller writing its own strip) is the same duplication somewhere nobody would
    * think to look when the suffix format changes.
    *
    * The eight digits are written out rather than quantified: anorm reads `{8}` as a bind parameter
    * named `8` and fails the statement before Postgres ever sees the pattern.
    */
  def normalizeSql(column: String): String =
    s"regexp_replace($column, '-[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$$', '')"

  /** Whether this table can price `model`. A model it cannot price is not called: [[ClaudeClient]]
    * refuses the request before it is sent, so a new model is priced here before it is used, and a
    * budget is never measured against a call whose cost reads as $0.
    */
  def isPriced(model: ClaudeModel): Boolean = isPriced(model.toString)

  def isPriced(model: String): Boolean = Rates.contains(normalize(model))

  /** USD cost of one response, read straight off what the API reported -- `None` when the model is
    * not priced (see [[isPriced]]).
    *
    * The overload every caller holding a live [[ClaudeUsage]] should reach for, rather than unpacking
    * the four token counts itself: which usage field carries the 1-hour write, and what an absent
    * `cache_creation` means, are decisions that belong here once instead of at every call site. An
    * absent `cache_creation` is 0 rather than unknown -- the API omits the object when the request
    * placed no cache breakpoint, and a response that wrote nothing wrote nothing at either TTL. The
    * tier comes off the same usage, so a call site that starts batching is priced correctly without
    * touching anything here (ISS-2064).
    */
  def costUsd(model: String, usage: ClaudeUsage): Option[BigDecimal] = {
    costUsd(
      model = model,
      inputTokens = usage.inputTokens,
      outputTokens = usage.outputTokens,
      cacheCreationInputTokens = usage.cacheCreationInputTokens.getOrElse(0L),
      cacheCreation1hInputTokens = usage.cacheCreation.map(_.ephemeral1hInputTokens).getOrElse(0L),
      cacheReadInputTokens = usage.cacheReadInputTokens.getOrElse(0L),
      serviceTier = ClaudeServiceTiers.fromApi(usage.serviceTier),
    )
  }

  /** USD cost of one request's usage, from token counts alone -- what a rollup and a repricer
    * have, since they read columns rather than a live response.
    *
    * `None` for a model id [[Rates]] does not carry (a new release not yet added). The caller says
    * what that means where it knows: [[ClaudeClient]] refuses to call a model it cannot price (see
    * [[isPriced]]), and an aggregate over stored rows decides for itself whether an unpriced row is
    * $0 or an error. A $0 here would be indistinguishable from a model nobody called (ISS-1925).
    *
    * `cacheCreation1hInputTokens` is the SUBSET of `cacheCreationInputTokens` written at the 1-hour
    * TTL, not a separate total to add on: the API's `cache_creation_input_tokens` always equals its
    * `cache_creation.ephemeral_5m + ephemeral_1h` (verified against the live API, 2026-08-11), so the
    * 5-minute part priced here is the remainder. Passing 0 is therefore the honest answer for a caller
    * that never asks for the longer TTL, and for every row written before ISS-1942.
    *
    * `serviceTier` has NO DEFAULT VALUE, on purpose. A default is precisely the bug this parameter
    * exists to prevent: a future call site that starts batching and forgets the argument would
    * over-report by 2x, silently, until somebody reconciled a bill against it. Required makes that
    * a compile error instead. Pass `None` only when the tier genuinely is not known -- see
    * [[tierMultiplier]] for what that costs.
    */
  def costUsd(
    model: String,
    inputTokens: Long,
    outputTokens: Long,
    cacheCreationInputTokens: Long,
    cacheCreation1hInputTokens: Long,
    cacheReadInputTokens: Long,
    serviceTier: Option[KnownClaudeServiceTier],
  ): Option[BigDecimal] = {
    Rates.get(normalize(model)).map { rate =>
      // Clamped rather than trusted: the subset relation is an invariant of what the API reports,
      // and a row that somehow violates it must not price the 5-minute part NEGATIVE and hand back
      // a total cheaper than the same usage with no caching at all.
      val fiveMinuteTokens = (cacheCreationInputTokens - cacheCreation1hInputTokens).max(0L)
      val listPrice = BigDecimal(inputTokens) * rate.inputPerM +
        BigDecimal(outputTokens) * rate.outputPerM +
        BigDecimal(fiveMinuteTokens) * rate.cacheWrite5mPerM +
        BigDecimal(cacheCreation1hInputTokens) * rate.cacheWrite1hPerM +
        BigDecimal(cacheReadInputTokens) * rate.cacheReadPerM
      listPrice * tierMultiplier(serviceTier) / PerMillion
    }
  }
}
