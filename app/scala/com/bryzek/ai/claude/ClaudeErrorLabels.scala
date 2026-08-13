package com.bryzek.ai.claude

/** How a [[com.bryzek.claude.models.ClaudeError]] says WHERE it came from.
  *
  * Two different ids can appear in one failure and they answer different questions:
  *
  *   - the LOCAL id ([[local]]) is minted here, before anything is sent. It is the primary key of the audit row a
  *     [[ClaudeStore]] writes, so it is how a failure is joined back to the request that produced it -- and it exists
  *     whether or not a socket was ever opened.
  *   - the PROVIDER id ([[providerSuffix]]) is Anthropic's own `request-id` response header. It means nothing to this
  *     codebase and everything to Anthropic's logs, so it is the one to quote in a support conversation.
  *
  * Neither can be looked up where the other lives, and until ISS-2542 they were indistinguishable: the local id was
  * formatted `[Request ID: req-<uuid>]`, which is the shape Anthropic uses (`req_...`) down to the prefix, and it was
  * attached identically to a failure that never left the process. A [[TestClaudeClient]] failure -- `Could not identify
  * json schema from object [Request ID: req-c6da...]`, raised on an output format missing from
  * `ClaudeOutputFormats.all`, no socket opened and nothing spent -- was therefore triaged as a live API call escaping a
  * spec built to make none, and escalated as a cost/leak incident (ISS-2522). A separate session read the same shape
  * quoted in a plan document and told itself to check the id "against API logs", where it does not exist.
  *
  * So each id now names whose it is, and a failure that never reached the API says so.
  */
private[claude] object ClaudeErrorLabels {

  /** Prefix on the correlation id this library mints per request.
    *
    * Deliberately NOT `req`: that is Anthropic's own prefix, and the id travels far from the message that carries it
    * -- it is `claude.requests.id` in the audit table, the `claude_request_id` log key, and whatever a triage note
    * copies out of either. It has to be unmistakable standing alone, not only inside [[local]]'s label.
    */
  val LocalIdPrefix: String = "libai"

  /** The response header Anthropic returns its own request id in. */
  val ProviderIdHeader: String = "request-id"

  /** How an error labels the request this library minted for it. `simulated` is the single most useful fact about an
    * in-process failure, so it is stated rather than left to be inferred from an id that looks like every other one.
    */
  def local(id: String, simulated: Boolean): String = {
    val note = if (simulated) "; simulated client, no API call" else ""
    s"[lib-ai request $id$note]"
  }

  /** Anthropic's own request id for a response, or None when it carried no such header (every in-process double, and
    * any failure that never got a response at all).
    */
  def providerId(headers: Map[String, scala.collection.Seq[String]]): Option[String] =
    headers
      .collectFirst { case (name, values) if name.equalsIgnoreCase(ProviderIdHeader) => values }
      .flatMap(_.find(_.trim.nonEmpty))
      .map(_.trim)

  /** [[providerId]] rendered for appending to an error message, or "" when there is none. Labelled `anthropic` because
    * that is the only place it can be looked up.
    */
  def providerSuffix(id: Option[String]): String =
    id.map(v => s" [anthropic request-id: $v]").getOrElse("")

  /** [[providerSuffix]] straight from a response's headers. */
  def providerSuffix(headers: Map[String, scala.collection.Seq[String]]): String =
    providerSuffix(providerId(headers))
}
