package org.squeryl.logging

import xml.Unparsed
import java.io.{FileOutputStream, PrintStream}
import org.squeryl.PrimitiveTypeMode._

object BarChartRenderer {

  class Stat(val title: String, val xAxisLabel: String, val lines: Iterable[StatLine], measureFromLike: StatLine => String) {

    def queryLabelsJSArray =
      lines.map(sl => "'" + sl.statement.definitionOrCallSite + "'").mkString("[",",","]")

    def measuresJSArray =
      lines.map(measureFromLike(_)).mkString("[",",","]")
  }

  def generateStatSummary(staticHtmlFile: java.io.File, n: Int) = {



    val page =
      BarChartRenderer.page(
        new Stat(
          "Top "+n+" statements with longest avg",
          "avg time",
          StatsSchema.topRankingStatements(n, Measure.AvgExecTime),
          sl => sl.avgExecTime.toString),
        new Stat(
          "Top "+n+" most called statements",
          "invocation count",
          StatsSchema.topRankingStatements(n, Measure.InvocationCount),
          sl => sl.invocationCount.toString),
        new Stat(
          "Top "+n+" statements incurring most cummulative execution time",
          "cummulative execution time",
          StatsSchema.topRankingStatements(n, Measure.CumulativeExecutionTime),
          sl => sl.cumulativeExecutionTime.toString),
        new Stat(
          "Top "+n+" statements with highest avg row count",
          "avg row count",
          StatsSchema.topRankingStatements(n, Measure.AvgResultSetSize),
          sl => sl.avgRowCount.toString)
      )

    val ps = new PrintStream(new FileOutputStream(staticHtmlFile))
    ps.print(page)
    ps.close
  }

  val drawFunc = """
    function drawBarGraph(divId, chartTitle, statType, queryClasses, measure) {
              var data = new google.visualization.DataTable();

              data.addColumn('string', 'Z');
              data.addColumn('number', statType);

              data.addRows(queryClasses.length);

              for (var j = 0; j < queryClasses.length; ++j) {
                data.setValue(j, 0, queryClasses[j].toString());
                data.setValue(j, 1, measure[j]);
              }

              var v = new google.visualization.BarChart(document.getElementById(divId))

              v.draw(data,
                       {title: chartTitle,
                        width:600, height:400,
                        vAxis: {title: "Queries"},
                        hAxis: {title: statType}
                       }
                  );
    }
  """

  def funcCalls(stats: Seq[Stat]) = {
    val sb = new StringBuffer
    var i = 0
    for(s <- stats) {
      i += 1
      sb.append("drawBarGraph('chart")
      sb.append(i)
      sb.append("','")
      sb.append(s.title)
      sb.append("','")
      sb.append(s.xAxisLabel)
      sb.append("',")
      sb.append(s.queryLabelsJSArray)
      sb.append(",")
      sb.append(s.measuresJSArray)
      sb.append(");\n")
    }
    sb.toString
   }


  def page(stats: Stat*) =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
        <title>Performance profile of Squeryl queries</title>
        <script type="text/javascript" src="http://www.google.com/jsapi"></script>
        <script type="text/javascript">
          google.load('visualization', '1', {{packages: ['corechart']}});
        </script>
        <script type="text/javascript">

          {Unparsed(drawFunc)}

          function drawVisualization() {{
            {Unparsed(funcCalls(stats))}
          }}

          google.setOnLoadCallback(drawVisualization);
        </script>
      </head>
      <body style="font-family: Arial;border: 0 none;">
        <div id="chart1" style="width: 1000px; height: 400px;"></div>
        <div id="chart2" style="width: 1000px; height: 400px;"></div>
        <div id="chart3" style="width: 1000px; height: 400px;"></div>
        <div id="chart4" style="width: 1000px; height: 400px;"></div>
      </body>
    </html>

}