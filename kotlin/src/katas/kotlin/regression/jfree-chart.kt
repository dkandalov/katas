package katas.kotlin.regression

import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.ChartUtils
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.DateAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.ui.RectangleInsets
import org.jfree.data.time.FixedMillisecond
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.xy.XYDataset
import java.awt.Color
import java.io.File
import java.text.SimpleDateFormat
import javax.swing.JFrame
import javax.swing.JPanel
import kotlin.math.sin


fun main() {
    val chart = createChart(createDataset())
    JFrame("").apply {
        contentPane.add(createDemoPanel(chart))
        pack()
        isVisible = true
    }
    ChartUtils.saveChartAsJPEG(File("foo.jpeg"), chart, 600, 400)
}

fun createDemoPanel(chart: JFreeChart): JPanel {
    val panel = ChartPanel(chart, false)
    panel.fillZoomRectangle = true
    panel.isMouseWheelEnabled = true
    return panel
}

fun createChart(dataset: XYDataset): JFreeChart {
    val title = "Legal & General Unit Trust Prices"
    val xAxisLabel = "Date"
    val yAxisLabel = "Price Per Unit"
    val chart = ChartFactory.createTimeSeriesChart(title, xAxisLabel, yAxisLabel, dataset)
    chart.backgroundPaint = Color.WHITE

    val plot = chart.plot as XYPlot
    plot.backgroundPaint = Color.LIGHT_GRAY
    plot.domainGridlinePaint = Color.WHITE
    plot.rangeGridlinePaint = Color.WHITE
    plot.axisOffset = RectangleInsets(5.0, 5.0, 5.0, 5.0)
    plot.isDomainCrosshairVisible = true
    plot.isRangeCrosshairVisible = true
    val r = plot.renderer
    if (r is XYLineAndShapeRenderer) {
        r.defaultShapesVisible = true
        r.defaultShapesFilled = true
        r.drawSeriesLineAsPath = true
    }
    val axis = plot.domainAxis as DateAxis
    axis.dateFormatOverride = SimpleDateFormat("MMM-yyyy")
    return chart
}

private fun createDataset(): XYDataset = TimeSeriesCollection().apply {
    addSeries(TimeSeries("sin").apply {
        (1..1000).map {
            val time = FixedMillisecond(it.toLong())
            val value = sin(it.toDouble() / 100) * 100 + 100
            add(time, value)
        }
    })

/*
    TimeSeries("L&G European Index Trust").apply {
        add(Month(2, 2001), 181.8)
        add(Month(3, 2001), 167.3)
        add(Month(4, 2001), 153.8)
        add(Month(5, 2001), 167.6)
        add(Month(6, 2001), 158.8)
        add(Month(7, 2001), 148.3)
        add(Month(8, 2001), 153.9)
        add(Month(9, 2001), 142.7)
        add(Month(10, 2001), 123.2)
        add(Month(11, 2001), 131.8)
        add(Month(12, 2001), 139.6)
        add(Month(1, 2002), 142.9)
        add(Month(2, 2002), 138.7)
        add(Month(3, 2002), 137.3)
        add(Month(4, 2002), 143.9)
        add(Month(5, 2002), 139.8)
        add(Month(6, 2002), 137.0)
        add(Month(7, 2002), 132.8)
        addSeries(this)
    }
*/

    /*TimeSeries("L&G UK Index Trust").apply {
        add(Month(2, 2001), 129.6)
        add(Month(3, 2001), 123.2)
        add(Month(4, 2001), 117.2)
        add(Month(5, 2001), 124.1)
        add(Month(6, 2001), 122.6)
        add(Month(7, 2001), 119.2)
        add(Month(8, 2001), 116.5)
        add(Month(9, 2001), 112.7)
        add(Month(10, 2001), 101.5)
        add(Month(11, 2001), 106.1)
        add(Month(12, 2001), 110.3)
        add(Month(1, 2002), 111.7)
        add(Month(2, 2002), 111.0)
        add(Month(3, 2002), 109.6)
        add(Month(4, 2002), 113.2)
        add(Month(5, 2002), 111.6)
        add(Month(6, 2002), 108.8)
        add(Month(7, 2002), 101.6)
        addSeries(this)
    }*/
}
