import scala.io.StdIn._

object StudentRecordManager {

    def calculateGrade(percentage: Double): Char = {
        percentage match {
        case p if p >= 90 => 'A'
        case p if p >= 75 => 'B'
        case p if p >= 50 => 'C'
        case _            => 'D'
        }
    }


    def getStudentInfo: (String, Int, Int, Double, Char) = {
        val (name, marks, totalMarks) = getStudentInfoWithRetry
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = calculateGrade(percentage)
        (name, marks, totalMarks, percentage, grade)
    }

    def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
        val (name, marks, totalMarks, percentage, grade) = student
        println(s"Student Name: $name")
        println(s"Marks: $marks / $totalMarks")
        println(f"Percentage: $percentage%.2f%%")
        println(s"Grade: $grade")
    }

    def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
        val marksOpt = try {
        Some(marks.toInt)
        } catch {
            case _: NumberFormatException => None
        }
        val totalMarksOpt = try {
        Some(totalMarks.toInt)
        } catch {
            case _: NumberFormatException => None
        }

        if (name.isEmpty) {
            (false, Some("Name cannot be empty"))
        } else if (marksOpt.isEmpty || totalMarksOpt.isEmpty) {
            (false, Some("Marks and total possible marks must be valid integers"))
        } else {
            val marksInt = marksOpt.get
            val totalMarksInt = totalMarksOpt.get
            if (marksInt < 0 || marksInt > totalMarksInt) {
                (false, Some("Marks must be between 0 and total possible marks"))
            } else if (totalMarksInt <= 0) {
                (false, Some("Total possible marks must be greater than 0"))
            } else {
                (true, None)
            }
        }
    }


    def getStudentInfoWithRetry: (String, Int, Int) = {
        var isValid = false
        var name = ""
        var marks = ""
        var totalMarks = ""

        while (!isValid) {
            println("Enter student's name: ")
            name = readLine().trim
            println("Enter marks obtained: ")
            marks = readLine().trim
            println("Enter total possible marks: ")
            totalMarks = readLine().trim

            validateInput(name, marks, totalMarks) match {
                case (true, _) =>
                    isValid = true
                    return (name, marks.toInt, totalMarks.toInt)
                case (false, Some(errorMessage)) =>
                    println(s"Invalid input: $errorMessage")
            }
        }

        (name, marks.toInt, totalMarks.toInt)
    }

    def main(args: Array[String]): Unit = {
        val studentInfo = getStudentInfo
        printStudentRecord(studentInfo)
    }
}