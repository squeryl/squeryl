package org.squeryl.logging

import xml.Unparsed
import java.io.{FileOutputStream, PrintStream}


object BarChartRenderer {

  class Stat(val title: String, val xAxisLabel: String, val lines: Iterable[StatLine], measureFromLike: StatLine => String) {

    def queryLabelsJSArray =
      lines.map(sl => "'" + sl.statement.definingClass + "'").mkString("[",",","]")

    def measuresJSArray =
      lines.map(measureFromLike(_)).mkString("[",",","]")
  }

  def generateStatSummary(staticHtmlFile: java.io.File, n: Int) = {

    val topAvgTime =
      new Stat("Top "+n+" statements with longest avg", "avg time", StatsSchema.topRankingStatements(n, Measure.AvgExecTime), sl => sl.avgExecTime.toString)

    val topCount =
      new Stat("Top "+n+" most called statements", "invocation count", StatsSchema.topRankingStatements(n, Measure.InvocationCount), sl => sl.invocationCount.toString)

    val page =
      BarChartRenderer.page(topAvgTime, topCount)

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

  def script(topAvgRuntimes: Stat, topMostInvoked: Stat) =
      <script type="text/javascript">

        {Unparsed(drawFunc)}

        function drawVisualization() {{
          drawBarGraph('chart1', '{topAvgRuntimes.xAxisLabel}', '{topAvgRuntimes.title}',
                       {topAvgRuntimes.queryLabelsJSArray},
                       {topAvgRuntimes.measuresJSArray});

          drawBarGraph('chart2', '{topMostInvoked.xAxisLabel}', '{topMostInvoked.title}',
                       {topMostInvoked.queryLabelsJSArray},
                       {topMostInvoked.measuresJSArray})
        }}

        google.setOnLoadCallback(drawVisualization);
      </script>

  def page(topAvgRuntimes: Stat, topMostInvoked: Stat) =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
        <title>
          Google Visualization API Sample
        </title>
        <script type="text/javascript" src="http://www.google.com/jsapi"></script>
        <script type="text/javascript">
          google.load('visualization', '1', {{packages: ['corechart']}});
        </script>
        {script(topAvgRuntimes, topMostInvoked)}
      </head>
      <body style="font-family: Arial;border: 0 none;">
        <div id="chart1" style="width: 600px; height: 400px;"></div>
        <div id="chart2" style="width: 600px; height: 400px;"></div>
      </body>
    </html>

}