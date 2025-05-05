// Ciobanu Cristian
// Mario Stanisor
// Daniela Botu
// Miruna Gherasim

import java.io.{File, FileWriter}
import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.io.StdIn
import scala.util.Try
import scala.io.Source

case class Data(startTime: String, endTime: String, value: Double)

object Main {
  private val formatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  def loadCsv(name: String): Option[List[Data]] = {
    Try {
      val file = Source.fromFile(name)
      try {
        println(s"Loading file: $name")
        val lines = file.getLines()
        val dataList = lines.flatMap { line =>
          Try {
            val Array(start, end, value) = line.split(";").map(_.trim.stripPrefix("\"").stripSuffix("\""))
            if (value.isEmpty || value == "null") {
              throw new IllegalArgumentException("Value is empty or invalid")
            }
            Data(start, end, value.toDouble)
          }.toOption
        }.toList
        if (dataList.nonEmpty) Some(dataList) else None
      } finally {
        file.close()
      }
    }.recover {
      case ex =>
        println(s"Failed to load file $name: ${ex.getMessage}")
        None
    }.get
  }



  def filterByDateRange(data: List[Data], start: String, end: String): List[Data] = {
    val startDate = ZonedDateTime.parse(start, formatter)
    val endDate = ZonedDateTime.parse(end, formatter)
    data.filter { d =>
      val time = ZonedDateTime.parse(d.startTime, formatter)
      !time.isBefore(startDate) && !time.isAfter(endDate)
    }
  }

  def simulateOutputChange(data: List[Data], percentage: Double): List[Data] = {
    val multiplier = if (percentage >= 0) 1 + percentage / 100 else 1 - Math.abs(percentage) / 100
    data.map(d => d.copy(value = d.value * multiplier)) // adjust each value based on the percentage
  }


  def saveAdjustedCsv(name: String, data: List[Data]): Unit = {
    val fileName = s"adjusted_$name.csv"
    val writer = new FileWriter(new File(fileName))
    try {
      writer.write("startTime,endTime,value\n") 
      data.foreach { d =>
        writer.write(s"${d.startTime},${d.endTime},${d.value}\n")
      }
    } finally {
      writer.close()
    }
    println(s"Saved adjusted data to: $fileName")
  }


  def displayLoaded(label: String, data: List[Data]): Unit = {
    println(s"\n--- $label ---")
    if (data.isEmpty) {
      println("No data available for the specified range.")
      return
    }


    println("-" * 86)
    println(f"| ${"Start Time"}%20s | ${"End Time"}%20s | ${"Value"}%10s |")
    println("-" * 86)

    data.foreach { d =>
      println(f"| ${d.startTime}%20s | ${d.endTime}%20s | ${d.value}%10.2f |")
    }
    println("-" * 86)
  }


  def adjustData(data: List[Data], percentage: Double): List[Data] = {
    data.map(d => d.copy(value = d.value * (1 + percentage / 100)))
  }

  def mean(data: List[Data]): Double = if (data.isEmpty) 0.0 else data.map(_.value).sum / data.size

  def median(data: List[Data]): Double = {
    val sorted = data.map(_.value).sorted
    val n = sorted.length
    if (n == 0) 0.0 else if (n % 2 == 1) sorted(n / 2) else (sorted(n / 2 - 1) + sorted(n / 2)) / 2
  }

  def mode(data: List[Data]): List[Double] = {
    val freq = data.map(_.value).groupBy(identity).view.mapValues(_.size).toMap
    val max = freq.values.maxOption.getOrElse(0)
    freq.filter(_._2 == max).keys.toList
  }

  def range(data: List[Data]): Double = if (data.isEmpty) 0.0 else data.map(_.value).max - data.map(_.value).min

  def midrange(data: List[Data]): Double = if (data.isEmpty) 0.0 else (data.map(_.value).max + data.map(_.value).min) / 2

  def assessDataset(datasets: Map[String, String]): Unit = {
    println("Choose dataset to assess:")
    datasets.keys.zipWithIndex.foreach { case (k, i) => println(s"${i + 1}. $k") }
    val index = scala.io.StdIn.readLine("Enter number of dataset: ").toInt - 1
    val selectedLabel = datasets.keys.toList(index)
    val selectedFile = datasets(selectedLabel)
    val dataOpt = loadCsv(selectedFile)
    dataOpt match {
      case Some(data) =>
        val threshold = scala.io.StdIn.readLine("Enter threshold value: ").toDouble

        val exceedThreshold = data.filter(_.value > threshold)
        if (exceedThreshold.nonEmpty) {
          println(s"\nValues exceeding threshold ($threshold):")
          exceedThreshold.foreach { d =>
            println(s"Time: ${d.startTime}, Value: ${d.value}")
          }
        } else {
          println(s"No values exceed the threshold ($threshold).")
        }
      case None =>
        println("Failed to load CSV data.")
    }
  }


  def main(args: Array[String]): Unit = {
    val datasets = Map(
      "Nuclear Power" -> "nuclearData.csv",
      "Solar Forecast" -> "solarData.csv",
      "Wind Power" -> "windData.csv",
      "Hydro Power" -> "hydroData.csv"
    )

    try {

      val loadedData = datasets.map { case (label, fileName) =>
        label -> loadCsv(fileName).getOrElse(Nil)
      }

      var keepRunning = true
      while (keepRunning) {
        println("Enter the start date (YYYY-MM-DD) from 2025-01-01 to 2025-04-30 (or '0' to exit):")
        val startInput = scala.io.StdIn.readLine()
        if (startInput == "0") {
          keepRunning = false
        } else {
          val startDate = s"${java.time.LocalDate.parse(startInput)}T00:00:00+03:00"

          println("Enter the end date (YYYY-MM-DD) from 2025-01-01 to 2025-04-30:")
          val endInput = scala.io.StdIn.readLine()
          val endDate = s"${java.time.LocalDate.parse(endInput)}T23:59:59+03:00"

          val filteredData = loadedData.map { case (label, data) =>
            label -> filterByDateRange(data, startDate, endDate)
          }

          println("\nChoose functionality:")
          println("1. Display fetched data")
          println("2. Simulate increase/decrease")
          println("3. Data analysis (filter, sort, stats)")
          println("4. Assess dataset for values exceeding a threshold")
          println("0. Exit")
          val choice = scala.io.StdIn.readLine()

          if (choice == "0") {
            keepRunning = false
          } else if (choice == "1") {
            filteredData.foreach { case (label, data) =>
              displayLoaded(label, data)
            }
          } else if (choice == "2") {
            println("Choose dataset to adjust:")
            filteredData.keys.zipWithIndex.foreach { case (k, i) => println(s"${i + 1}. $k") }
            val index = scala.io.StdIn.readLine("Enter number of dataset: ").toInt - 1
            val selectedLabel = filteredData.keys.toList(index)
            println("Do you want to increase or decrease output? (i/d)")
            val direction = scala.io.StdIn.readLine().toLowerCase
            val percent = scala.io.StdIn.readLine("By how much? (e.g., 12): ").toDouble
            val adjustedData = simulateOutputChange(filteredData(selectedLabel), percent)

            saveAdjustedCsv(selectedLabel.replaceAll(" ", "_").toLowerCase, adjustedData)

            displayLoaded(s"$selectedLabel (Adjusted by $percent% $direction)", adjustedData)
          } else if (choice == "3") {
            println("Choose dataset to analyze:")
            filteredData.keys.zipWithIndex.foreach { case (k, i) => println(s"${i + 1}. $k") }
            val index = scala.io.StdIn.readLine("Enter number of dataset: ").toInt - 1
            val selectedLabel = filteredData.keys.toList(index)
            val selectedData = filteredData(selectedLabel)

            println("Select granularity: hourly / daily / weekly / monthly")
            val granularity = scala.io.StdIn.readLine().toLowerCase

            println("Filter by min value? (y/n)")
            val doFilter = scala.io.StdIn.readLine().toLowerCase == "y"
            val filteredDataForAnalysis = if (doFilter) {
              val threshold = scala.io.StdIn.readLine("Minimum value: ").toDouble
              selectedData.filter(_.value >= threshold)
            } else {
              selectedData
            }

            println("Sort data? (y/n)")
            val doSort = scala.io.StdIn.readLine().toLowerCase == "y"
            val sortedData = if (doSort) {
              val field = scala.io.StdIn.readLine("Sort by: value / start / end: ").toLowerCase
              val dir = scala.io.StdIn.readLine("Direction: asc / desc: ").toLowerCase
              field match {
                case "value" => if (dir == "asc") filteredDataForAnalysis.sortBy(_.value) else filteredDataForAnalysis.sortBy(-_.value)
                case "start" => if (dir == "asc") filteredDataForAnalysis.sortBy(_.startTime) else filteredDataForAnalysis.sortBy(_.startTime).reverse
                case "end" => if (dir == "asc") filteredDataForAnalysis.sortBy(_.endTime) else filteredDataForAnalysis.sortBy(_.endTime).reverse
                case _ => filteredDataForAnalysis
              }
            } else {
              filteredDataForAnalysis
            }

            val grouped = groupByPeriod(sortedData, granularity)
            grouped.toSeq.sortBy(_._1).foreach { case (period, values) =>
              println(s"\n--- [$period] ---")
              println(f"Count: ${values.size}")
              println(f"Mean: ${mean(values)}%.2f")
              println(f"Median: ${median(values)}%.2f")
              println(f"Mode: ${mode(values).mkString(", ")}")
              println(f"Range: ${range(values)}%.2f")
              println(f"Midrange: ${midrange(values)}%.2f")
            }
          } else if (choice == "4") {
            assessDataset(datasets)
          } else {
            println("Invalid choice. Please select a valid option.")
          }
        }
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
    }
  }
  def groupByPeriod(data: List[Data], granularity: String): Map[String, List[Data]] = {
    val formatter = java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
    data.groupBy { d =>
      val time = java.time.ZonedDateTime.parse(d.startTime, formatter)
      granularity match {
        case "hourly" => time.truncatedTo(java.time.temporal.ChronoUnit.HOURS).toString
        case "daily" => time.toLocalDate.toString
        case "weekly" => s"${time.getYear}-W${time.get(java.time.temporal.IsoFields.WEEK_OF_WEEK_BASED_YEAR)}"
        case "monthly" => s"${time.getYear}-${time.getMonthValue}"
        case _ => "unknown"
      }
    }
  }
}
