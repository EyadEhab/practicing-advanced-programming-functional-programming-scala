import scala.io.StdIn.readLine
import scala.util.Random

object MusicBotApp {
  def simpleSongBot(): Unit = {

    val songs = List(
      ("rock", List("Don't Stop Believin'", "Another Brick in the Wall", "Bohemian Rhapsody")),
      ("pop", List("Hello", "Firework"))
    )

    val positiveSongs = List(
      "Don't Stop Believin' - Journey",
      "Happy - Pharrell Williams",
      "Walking on Sunshine - Katrina and the Waves",
      "Firework - Katy Perry",
      "Good Vibrations - The Beach Boys"
    )

    val lyrics = List(
      ("rock", List(
        ("Just a small-town girl, livin' in a lonely world...", "Don't Stop Believin'"),
        ("We don't need no education...", "Another Brick in the Wall"),
        ("Is this the real life? Is this just fantasy?", "Bohemian Rhapsody")
      )),
      ("pop", List(
        ("Hello from the other side...", "Hello"),
        ("Cause baby you're a firework...", "Firework")
      ))
    )

    var correctAnswers = 0
    var totalQuestions = 0

    def greeting(): Unit = {
      println("👋 Hello! My name is Music Bot. How are you today?")
    }

    def songRecommendationFlow(): Unit = {
      println("🎧 Let's find you a song!")
      
      // Ask for a valid genre with error handling
      var genre: String = ""
      var validGenre = false
      while (!validGenre) {
        print("👉 What genre do you like? (rock/pop): ")
        genre = readLine().toLowerCase()

        // Check if the genre is valid
        if (List("rock", "pop").contains(genre)) {
          validGenre = true
        } else {
          println("⚠ Invalid genre! Please choose 'rock' or 'pop'.")
        }
      }

      print("👉 Do you prefer a male or female singer? (male/female/any): ")
      val gender = readLine().toLowerCase()

      print("👉 What decade do you prefer? (e.g., 80s, 90s, 2000s): ")
      val decade = readLine()

      val foundSongs = songs.find(_._1 == genre)
      foundSongs match {
        case Some((_, songList)) =>
          val song = songList(Random.nextInt(songList.length))
          println(s"🎶 Based on your taste ($genre, $gender vocals, $decade vibe), I recommend: $song")
        case None =>
          println("⚠ Sorry, I couldn't find any songs in that genre.")
      }
    }

    def cheerUpUser(): Unit = {
      val song = positiveSongs(Random.nextInt(positiveSongs.length))
      println(s"💖 I'm sorry to hear that! Here's a cheerful song to lift your mood: 🎶 $song 🎶")
    }

    def songQuiz(): Unit = {
      // Ask for a valid genre with error handling
      var genre: String = ""
      var validGenre = false
      while (!validGenre) {
        println("🎤 What genre do you want? (e.g., rock, pop):")
        genre = readLine().toLowerCase()

        // Check if the genre is valid
        if (List("rock", "pop").contains(genre)) {
          validGenre = true
        } else {
          println("⚠ Invalid genre! Please choose 'rock' or 'pop'.")
        }
      }
      
      val foundLyrics = lyrics.find(_._1.toLowerCase == genre)

      foundLyrics match {
        case Some((_, lyricList)) =>
          val questions = Random.shuffle(lyricList).take(3)
          var questionCount = 1
          var remainingQuestions = questions.length

          var continueQuiz = true
          val questionIterator = questions.iterator

          while (questionIterator.hasNext && continueQuiz) {
            val (lyric, correctSong) = questionIterator.next()

            println(s"\nQuestion $questionCount:")
            println("🎼 Guess the song for the lyric:")
            println("\"" + lyric + "\"")

            val correct = correctSong
            val wrongChoices = songs.flatMap(_._2).filterNot(_ == correct).take(3)
            val allChoices = Random.shuffle(correct +: wrongChoices).zip(List("a", "b", "c", "d"))

            allChoices.foreach { case (title, letter) =>
              println(s"$letter) $title")
            }

            print("👉 Your answer (a/b/c/d): ")
            val userInput = readLine().toLowerCase
            totalQuestions += 1

            val selected = allChoices.find(_._2 == userInput).map(_._1)

            selected match {
              case Some(songTitle) =>
                if (songTitle.toLowerCase == correct.toLowerCase) {
                  println("✅ Correct!")
                  correctAnswers += 1
                } else {
                  println(s"❌ Incorrect. Correct answer was: $correct")
                }
              case None =>
                println("⚠ Invalid choice.")
            }

            questionCount += 1
            remainingQuestions -= 1

            if (remainingQuestions > 0) {
              println(s"\n❓ Do you want to continue? ($remainingQuestions questions left) (yes/no):")
              val continueInput = readLine().toLowerCase
              if (continueInput.contains("no")) {
                println("🔚 Ending the quiz early. Returning to main menu!")
                continueQuiz = false
              }
            } else {
              println("✅ That was the last question! Great job!")
            }
          }

        case None =>
          println("⚠ No lyrics found for that genre.")
      }

      // After the quiz ends, suggest showing analytics
      println("\n🎉 Quiz complete!")
      println("Would you like to see your quiz analytics? (yes/no): ")
      val showAnalytics = readLine().toLowerCase
      if (showAnalytics.contains("yes")) {
        analytics()
      } else {
        println("🔙 Returning to the main menu.")
      }
    }

    def analytics(): Unit = {
      println("📊 Quiz Analytics:")
      println(s"- Total Questions: $totalQuestions")
      println(s"- Correct Answers: $correctAnswers")
      if (totalQuestions > 0) {
        val percentage = (correctAnswers.toDouble / totalQuestions * 100).toInt
        println(s"- Accuracy: $percentage%")
      }
    }

    def showCapabilities(): Unit = {
      println(
        """🤖 Here's what I can do:
- Recommend you a song based on your taste 🎧
- Test your knowledge with a song lyrics quiz 🎤
- Show your quiz performance and stats 📊
- Cheer you up with a happy song 💖
- Just chat with you about music 🎶
What would you like to try?"""
      )
    }

    def handleBoredUser(): Unit = {
      println("😴 Sounds like you're bored! Here's what we can do:")
      showCapabilities()
    }

    def respondToUser(input: String): Unit = {
      val lowerInput = input.toLowerCase

      val sadnessKeywords = List("sad", "not good", "bad", "upset", "depressed")
      val boredKeywords = List("bored", "what else", "what do you have", "anything else")
      val recommendKeywords = List("recommend", "song")
      val quizKeywords = List("start", "quiz", "game")
      val analyticsKeywords = List("analytics", "score", "percentage")
      val positiveKeywords = List("good", "great", "fine", "awesome")
      val exitKeywords = List("bye", "exit", "quit")
      val noInterestKeywords = List("no", "nah", "not interested", "don't want")

      val matchedCategories = List(
        if (sadnessKeywords.exists(lowerInput.contains)) Some("sadness") else None,
        if (boredKeywords.exists(lowerInput.contains)) Some("boredom") else None,
        if (recommendKeywords.exists(lowerInput.contains)) Some("recommend") else None,
        if (quizKeywords.exists(lowerInput.contains)) Some("quiz") else None,
        if (analyticsKeywords.exists(lowerInput.contains)) Some("analytics") else None,
        if (positiveKeywords.exists(lowerInput.contains)) Some("positive") else None,
        if (exitKeywords.exists(lowerInput.contains)) Some("exit") else None,
        if (noInterestKeywords.exists(lowerInput.contains)) Some("no_interest") else None
      ).flatten

      if (matchedCategories.size > 1) {
        println("⚠ It looks like you mentioned multiple things. Could you please tell me what you'd like to do first?")
        showCapabilities()
      } else if (matchedCategories.isEmpty) {
        println("🤖 I'm not sure how to respond to that. You can ask me to recommend a song, start a quiz, check your analytics, cheer you up, or just chat!")
      } else {
        matchedCategories.head match {
          case "sadness" => cheerUpUser()
          case "boredom" => handleBoredUser()
          case "recommend" => songRecommendationFlow()
          case "quiz" => songQuiz()
          case "analytics" => analytics()
          case "positive" =>
            println("😊 Wonderful! Would you like to play a game or get a song recommendation?")
          case "exit" =>
            println("👋 Goodbye! Have a musical day!")
            System.exit(0)
          case "no_interest" =>
            println("✅ Understood. How can I be of service?")
        }
      }
    }

    // Main Loop
    greeting()
    var running = true
    while (running) {
      print("\n💬 You: ")
      val userInput = readLine()
      respondToUser(userInput)
    }
  }

  def main(args: Array[String]): Unit = {
    simpleSongBot()
  }
}
