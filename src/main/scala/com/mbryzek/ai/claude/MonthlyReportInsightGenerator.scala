package claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import claude.ClaudeClient
import com.bryzek.claude.v0.models.{ClaudeModel, ClaudeRequest, ClaudeRole}
import db.InternalGroupsDao
import org.joda.time.LocalDate
import reports.InterimMonthlyReportUtil

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

sealed trait MonthlyReport
object MonthlyReport {
  case object NoDataAvailable extends MonthlyReport
  case class Report(report: String) extends MonthlyReport
}

class MonthlyReportInsightGenerator @Inject() (
  client: ClaudeClient,
  internalGroupsDao: InternalGroupsDao,
  monthlyReportUtil: InterimMonthlyReportUtil
) {
  private val Model = ClaudeModel.ClaudeOpus4120250805

  private val Prompt = """
  You are a financial insights assistant. Given a summary of a user's monthly spending (including top expense categories and largest month-over-month changes), your goal is to produce exactly one actionable insight that would help the user make better financial decisions next month.

The insight should be:

Focused on a single category or spending pattern.
Framed as a recommendation or next step.
Prioritized based on impact (size of expense or change) and relevance.

IMPORTANT: You must respond in JSON format with the following structure:
{
  "steps": [
    {
      "explanation": "Brief explanation of your analysis step",
      "output": "The result or finding from this step"
    }
  ],
  "comments": ["Your single actionable insight here"]
}

IMPORTANT: DO NOT wrap the json with "```json" - only output the JSON
"""

  def generate(groupId: String, month: LocalDate)(implicit ec: ExecutionContext): Future[ValidatedNec[String, MonthlyReport]] = {
    val data = monthlyReportData(groupId, month)
    if (data.isEmpty) {
      Future.successful(MonthlyReport.NoDataAvailable.validNec)
    } else {
      internalGroupsDao.findById(groupId) match {
        case None => Future.successful("Group not found".invalidNec)
        case Some(group) => {
          client.chatSingleInsight(group.environment, ClaudeRequest(
            model = Model,
            messages = Seq(
              client.makeClaudeMessage(ClaudeRole.Assistant, Prompt),
              client.makeClaudeMessage(ClaudeRole.User, data*),
              client.makeClaudeMessage(ClaudeRole.User, "Generate an insight for the month of " + month)
            )
          ))(using ec).map(_.leftMap(_.map(_.message)).map(MonthlyReport.Report.apply))
        }
      }
    }
  }

  private def monthlyReportData(groupId: String, month: LocalDate): Seq[String] = {
    monthlyReportUtil.generateInterim(groupId, month) match {
      case Invalid(e) => Nil
      case Valid(report) => {
        Seq(
          s"Data for ${report.month}",
          s"Top Expenses: " + report.topExpenses.map(_.toString).mkString(", "),
          s"Biggest Changes: " + report.topChangesFromPriorMonth.map(_.toString).mkString(", "),
        )
      }
    }
  }

}
