package dataset

import java.text.SimpleDateFormat
import java.util.{Date, SimpleTimeZone}

import dataset.util.Commit.{Commit, File}

import scala.collection.mutable.Map

/**
  * Use your knowledge of functional programming to complete the following functions.
  * You are recommended to use library functions when possible.
  *
  * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
  * When asked for dates, use the `commit.commit.committer.date` field.
  *
  * This part is worth 40 points.
  */
object Dataset {

    /** Q16 (5p)
      * For the commits that are accompanied with stats data, compute the average of their additions.
      * You can assume a positive amount of usable commits is present in the data.
      * @param input the list of commits to process.
      * @return the average amount of additions in the commits that have stats data.
      */
    def avgAdditions(input: List[Commit]): Int = {

        def helperAverage (intList: List[Int], length: Int, sum: Int): Int = {
            intList match {
                case Nil => sum / length
                case x :: tail => helperAverage (tail, length + 1, sum + x)
            }
        }
        def helperList (commitList: List[Commit]): List[Int] = {
            val intList: List[Int] = commitList.map(commit => commit.stats.get.additions)
            intList
        }
        helperAverage (helperList(input), 0, 0)
    }

    /** Q17 (8p)
      * Find the hour of day (in 24h notation) during which the most javascript (.js) files are changed in commits.
      * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
      * @param input list of commits to process.
      * @return the hour and the amount of files changed during this hour.
      */
    def jsTime(input: List[Commit]): (Int, Int) = {

        def helperTime(commitList: List[Commit]): List[Int] = {

            val jsList: List[Commit] = commitList.filter(commit => helperJson(commit))

            val fileList: List[Date] = helperUnpack(jsList).flatten
            val intList: List[Int] = fileList.map(date => helperTimezone(date))

//            val intList: List[Int] = fileList.map(commit => commit.commit.committer.date).map(date => helperTimezone(date))

//            val fileList = jsList.map(commit => commit.commit.author.date).map(date => helperTimezone(date))

            intList
        }

        def helperUnpack(commitList: List[Commit]) : List[List[Date]] = commitList match {
            case Nil => Nil
            case (x:Commit) :: (tail: List[Commit]) => helperCommitToDate(x) :: helperUnpack(tail)
        }

        def helperCommitToDate(input: Commit): List[Date] = {
            val fileListRaw: List[File] = input.files
            val fileListFilterd: List[File] = helperFilterFileListJS(fileListRaw)
            val date: Date = input.commit.committer.date
            val result = fileListFilterd.map(file => date)
            result
        }

        def helperFilterFileListJS(input: List[File]): List[File] = input match{
            case Nil => Nil
            case (file: File) :: (tail: List[File]) =>
                if(getFileExtension3(file) == "js") file :: helperFilterFileListJS(tail)
                else helperFilterFileListJS(tail)
        }

        def helperTimezone(dateIn: Date): Int = {
            val tempTime = new SimpleDateFormat("H")
            val tempTimeZone = new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC")
            tempTime.setTimeZone(tempTimeZone)
            val dateTemp2 = tempTime.format(dateIn).toInt
            dateTemp2
        }

        def helperHour(intList: List[Int]): (Int, Int) = {
            val intTuple = intList.groupBy(identity).mapValues(_.size).s
            val result = intTuple.maxBy(_._2)
            result
        }

        def helperJson(input: Commit): Boolean = {
            if(getFileExtension(input).contains("js")) true else false
        }

        def getFileExtension(input: Commit): List[String] = {
            val tempFiles: List[File] = input.files
            getFileExtension2(tempFiles)
        }

        def getFileExtension2(input: List[File]): List[String] = input match {
            case Nil => Nil
            case x :: tail => getFileExtension3(x) :: getFileExtension2(tail)
        }

        def getFileExtension3(input: File): String = {
            val tempFileType1: String = input.filename.get
            val pattern = """\.js$""".r
            val tempFileType2: Option[String] = pattern.findFirstIn(tempFileType1)
            val tempFileType = tempFileType2.getOrElse("FFalse").substring(1)
            tempFileType
        }

        helperHour(helperTime(input))
    }

    /** Q18 (9p)
      * For a given repository, output the name and amount of commits for the person
      * with the most commits to this repository.
      * @param input the list of commits to process.
      * @param repo the repository name to consider.
      * @return the name and amount of commits for the top committer.
      */
    def topCommitter(input: List[Commit], repo: String): (String, Int) = {
        val listName: List[String] = input.filter(commit => commit.url.contains(repo)).map(commit => commit.commit.author.name)
        val nameTuple = listName.groupBy(identity).mapValues(_.size)
        val result = nameTuple.maxBy(_._2)
        result
    }

    /** Q19 (9p)
      * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
      * Leave out all repositories that had no activity this year.
      * @param input the list of commits to process.
      * @return a map that maps the repo name to the amount of commits.
      *
      * Example output:
      * Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
      */
    def commitsPerRepo(input: List[Commit]): Map[String, Int] = {

        val repo2019 = input.filter(commit => (helperDate(commit.commit.committer.date) == 2019)).map(commit => repositoryHelper(commit))
//        val repo2019 = input.map(commit => repositoryHelper(commit))
        val result = repo2019.groupBy(identity).mapValues(_.size)
        collection.mutable.Map() ++ result
    }

    def repositoryHelper(input: Commit): String = {
        val urlTemp: String = input.url
        val pattern = """(/repos)(/.*)""".r
        val fileExtensionTemp: Option[String] = pattern.findFirstIn(urlTemp)
        val fileExtension = fileExtensionTemp.getOrElse("SomethingWentWrong").substring(7)
        val fileSplit = fileExtension.split("/")
        val fileResult = fileSplit(0) + "/" + fileSplit(1)
        fileResult
    }

    def helperDate(dateIn: Date): Int = {
        val tempTime = new SimpleDateFormat("yyyy")
        val tempTimeZone = new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC")
        tempTime.setTimeZone(tempTimeZone)
        val dateTemp2 = tempTime.format(dateIn).toInt
        dateTemp2
    }

    /** Q20 (9p)
      * Derive the 5 file types that appear most frequent in the commit logs.
      * @param input the list of commits to process.
      * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
      */
    def topFileFormats(input: List[Commit]): List[(String, Int)] = {

        def getFileExtension(input: Commit): List[String] = {
            val tempFiles: List[File] = input.files
            getFileExtension2(tempFiles)
        }

        def getFileExtension2(input: List[File]): List[String] = input match {
            case Nil => Nil
            case x :: tail => getFileExtension3(x) :: getFileExtension2(tail)
        }

        def getFileExtension3(input: File): String = {
            val tempFileType1: String = input.filename.get
            val pattern = """\.[A-Za-z0-9]+$""".r
            val tempFileType2: Option[String] = pattern.findFirstIn(tempFileType1)
            val tempFileType = tempFileType2.getOrElse("SomethingWentWrong").substring(1)
            tempFileType
        }

        val listListName: List[List[String]] = input.map(commit => getFileExtension(commit))
        val listName: List[String] = listListName.flatten
        val nameTuple = listName.groupBy(identity).mapValues(_.size)
        val temp: List[(String, Int)] = nameTuple.toList
        val result = temp.sortBy(_._2).reverse
        result.take(5)
    }
}
