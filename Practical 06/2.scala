import scala.io.StdIn

object StudentManager {

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some(s"Marks should be between 0 and $totalMarks."))
    } else if (totalMarks <= 0) {
      (false, Some("Total marks should be positive."))
    } else {
      (true, None)
    }
  }

  // Function to get student information
  def getStudentInfo: (String, Int, Int, Double, Char) = {
    printf("Enter student's name:")
    val name = StdIn.readLine().trim
    
    printf("Enter student's marks:")
    val marks = StdIn.readInt()
    
    printf("Enter total possible marks:")
    val totalMarks = StdIn.readInt()

    // Validate input
    validateInput(name, marks, totalMarks) match {
      case (true, _) =>
        // Calculate percentage
        val percentage = (marks.toDouble / totalMarks) * 100
        // Determine grade
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _ => 'D'
        }
        (name, marks, totalMarks, percentage, grade)
        
      case (false, Some(errorMessage)) =>
        println("Error: " + errorMessage)
        ("", 0, 0, 0.0, 'D')

       case (false, None) =>
        println("Unexpected error occurred.")
        ("", 0, 0, 0.0, 'D')
    }

  }

  // print student record
  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println("Student Name:"+ name)
    println("Marks: "+marks)
    println("Total Marks: "+totalMarks)
    println("Percentage: "+ percentage)
    println("Grade: "+ grade)
  }

  //  get student info with retry
  def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
    var validInput = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

    while (!validInput) {
      studentInfo = getStudentInfo
      if (!validInput) {
        println("Please enter valid data.")
      }
    }
    
    studentInfo
  }

  def main(args: Array[String]): Unit = {
    
    val studentRecord = getStudentInfoWithRetry
    printStudentRecord(studentRecord)
  }
}
