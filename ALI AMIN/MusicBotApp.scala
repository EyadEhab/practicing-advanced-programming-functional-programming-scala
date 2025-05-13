import scala.io.StdIn.readLine
import scala.util.Random

object MusicBotApp {
  def simpleSongBot(): Unit = {
    val songs = List(
      ("rock", List(
        "Don't Stop Believin' - Journey (1981)",
        "Another Brick in the Wall - Pink Floyd (1979)",
        "Bohemian Rhapsody - Queen (1975)"
      )),
      ("pop", List(
        "Hello - Adele (2015)",
        "Firework - Katy Perry (2010)",
        "Shape of You - Ed Sheeran (2017)"
      )),
      ("romantic", List(
        "Perfect - Ed Sheeran (2017)",
        "All of Me - John Legend (2013)",
        "Thinking Out Loud - Ed Sheeran (2014)"
      )),
      ("jazz", List(
        "Take Five - Dave Brubeck (1959)",
        "What a Wonderful World - Louis Armstrong (1967)",
        "So What - Miles Davis (1959)"
      )),
      ("chill", List(
        "Sunset Lover - Petit Biscuit (2015)",
        "Weightless - Marconi Union (2011)"
      )),
      ("classic", List(
        "Fur Elise - Beethoven (1810)",
        "Moonlight Sonata - Beethoven (1801)",
        "Canon in D - Pachelbel (1680)"
      )),
      ("electronic", List(
        "Levels - Avicii (2011)",
        "One More Time - Daft Punk (2000)",
        "Clarity - Zedd ft. Foxes (2012)"
      )),
      ("country", List(
        "Humble and Kind - Tim McGraw (2016)",
        "Body Like a Back Road - Sam Hunt (2017)",
        "Tennessee Whiskey - Chris Stapleton (2015)"
      )),
      ("hiphop", List(
        "Lose Yourself - Eminem (2002)",
        "Juicy - The Notorious B.I.G. (1994)",
        "Nuthin' but a 'G' Thang - Dr. Dre ft. Snoop Dogg (1992)"
      ))
    )

    val positiveSongs = List(
      ("Don't Stop Believin' - Journey", "Just a small-town girl, livin' in a lonely world..."),
      ("Happy - Pharrell Williams", "It might seem crazy what I'm about to say..."),
      ("Walking on Sunshine - Katrina and the Waves", "I'm walking on sunshine, whoa...")
    )

    val greetings = List(
      "👋 Hey there! I'm Music Bot. How's your day going?",
      "🎵 Hi! Ready to dive into the world of music?",
      "🤖 Hello, music lover! How are you feeling today?"
    )

    val lyrics = List(
      ("rock", List(
        ("Just a small-town girl, livin' in a lonely world...", "Don't Stop Believin'"),
        ("We don't need no education...", "Another Brick in the Wall")
      )),
      ("pop", List(
        ("Hello from the other side...", "Hello"),
        ("Cause baby you're a firework...", "Firework")
      )),
      ("romantic", List(
        ("Darling, just kiss me slow, your heart is all I own...", "Perfect"),
        ("What would I do without your smart mouth...", "All of Me")
      )),
      ("jazz", List(
        ("Take five, take five...", "Take Five"),
        ("I see trees of green, red roses too...", "What a Wonderful World")
      )),
      ("chill", List(
        ("(Instrumental)", "Sunset Lover"),
        ("(Instrumental)", "Weightless")
      )),
      ("classic", List(
        ("(Instrumental)", "Fur Elise"),
        ("(Instrumental)", "Moonlight Sonata")
      )),
      ("electronic", List(
        ("Oh, sometimes I get a good feeling, yeah...", "Levels"),
        ("One more time...", "One More Time")
      )),
      ("country", List(
        ("You gotta stay humble and kind...", "Humble and Kind"),
        ("Body like a back road, drivin' with my eyes closed...", "Body Like a Back Road")
      )),
      ("hiphop", List(
        ("Look, if you had one shot, or one opportunity...", "Lose Yourself"),
        ("It was all a dream, I used to read Word Up! magazine...", "Juicy")
      ))
    )

    var correctAnswers = 0
    var totalQuestions = 0
    var hadQuiz = false

    def greeting(): Unit = {
      println(greetings(Random.nextInt(greetings.length)))
      println("You can ask for a song recommendation, start a quiz, or just chat!")
    }

    def checkForExit(input: String): Boolean = {
      val exitWords = Set("exit", "quit", "bye", "goodbye", "stop", "end", "close")
      if (exitWords.contains(input.toLowerCase)) {
        println("👋 Goodbye! Have a musical day!")
        System.exit(0)
        true
      } else {
        false
      }
    }

    def readLineWithExit(prompt: String = ""): String = {
      if (prompt.nonEmpty) print(prompt)
      val input = readLine()
      if (checkForExit(input)) "" else input
    }

    def songRecommendationFlow(): Unit = {
      println("🎧 Let's find you a song!")

      var genre: String = ""
      var validGenre = false
      while (!validGenre) {
        val input = readLineWithExit(s"👉 What genre do you like? (${songs.map(_._1).mkString("/")}): ")
        if (input.isEmpty) return
        genre = input.toLowerCase()
        if (songs.exists(_._1 == genre)) {
          validGenre = true
        } else {
          println(s"⚠ Invalid genre! Please choose from: ${songs.map(_._1).mkString(", ")}.")
        }
      }

      val decadeInput = readLineWithExit("👉 What decade are you interested in? (e.g., 80s, 90s, 2000s, 2010s, 2020s or 1980, 1990, etc.): ")
      if (decadeInput.isEmpty) return

      // Easier decade normalization: extract all numbers, use first 2 or 4 digits
      val normalizedDecade: Option[(Int, Int)] = {
        val digits = "[0-9]+".r.findAllIn(decadeInput).toList
        digits.headOption match {
          case Some(num) if num.length == 4 =>
            val start = num.toInt
            Some((start, start + 10))
          case Some(num) if num.length == 2 =>
            val decade = num.toInt
            val start = if (decade < 30) 2000 + decade else 1900 + decade
            Some((start, start + 10))
          case _ => None
        }
      }

      val foundSongs = songs.find(_._1 == genre)
      foundSongs match {
        case Some((_, songList)) =>
          val filteredSongs = normalizedDecade match {
            case Some((startYear, endYear)) =>
              songList.filter { titleYear =>
                val yearOpt = "\\((\\d{4})\\)".r.findFirstMatchIn(titleYear).map(_.group(1).toInt)
                yearOpt.exists(year => year >= startYear && year < endYear)
              }
            case None =>
              songList // If decade not recognized, show all
          }

          if (filteredSongs.nonEmpty) {
            val recommendedSong = filteredSongs(Random.nextInt(filteredSongs.length))
            println(s"🎶 Based on your taste ($genre, $decadeInput), I recommend: $recommendedSong")
            // Find the lyrics if available
            val lyricOption = lyrics.find(_._1 == genre).flatMap(_._2.find(_._2.toLowerCase.contains(recommendedSong.split(" - ").head.toLowerCase)))
            lyricOption.foreach(lyric => println(s"🎤 A snippet of the lyrics: \"${lyric._1}\""))
            println("💬 Did you like the suggestion? Want to hear another or try a quiz?")
          } else {
            println(s"⚠ Sorry, I couldn't find any $genre songs from the $decadeInput.")
          }
        case None =>
          println("⚠ Sorry, I couldn't find any songs in that genre.")
      }
    }

    def cheerUpUser(): Unit = {
      val (song, lyricsSnippet) = positiveSongs(Random.nextInt(positiveSongs.length))
      println(s"💖 I'm sorry to hear that! Here's a cheerful song to lift your mood: 🎶 $song 🎶")
      println(s"🎤 A little something to sing along to: \"$lyricsSnippet\"")
      println("Would you like to play a music quiz or get a song recommendation next?")
    }

    def songQuiz(): Unit = {
      hadQuiz = true
      var genre: String = ""
      var validGenre = false
      while (!validGenre) {
        val input = readLineWithExit(s"🎤 What genre do you want for the quiz? (${lyrics.map(_._1).mkString("/")}): ")
        if (input.isEmpty) return
        genre = input.toLowerCase()
        if (lyrics.exists(_._1 == genre)) {
          validGenre = true
        } else {
          println(s"⚠ Invalid genre! Please choose from: ${lyrics.map(_._1).mkString(", ")}.")
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

            // Find the full song info from the songs list
            val fullCorrectSong = songs.flatMap(_._2).find(_.split(" - ").head.equalsIgnoreCase(correctSong))
            val correctDisplay = fullCorrectSong.getOrElse(correctSong)
            
            // Get wrong choices (full song info)
            val wrongChoices = songs.flatMap(_._2)
              .filterNot(_.toLowerCase.contains(correctSong.toLowerCase))
              .filterNot(_.equalsIgnoreCase(correctDisplay))
              .distinct
              .take(3)
            
            val allChoicesWithCorrect = Random.shuffle(correctDisplay +: wrongChoices)
            val allChoices = allChoicesWithCorrect.zip(List("a", "b", "c", "d"))

            allChoices.foreach { case (title, letter) =>
              println(s"$letter) $title")
            }

            var validAnswer = false
            var selectedSong: Option[String] = None

            while (!validAnswer) {
              val userInput = readLineWithExit("👉 Your answer (a/b/c/d): ")
              if (userInput.isEmpty) return

              selectedSong = allChoices.find(_._2 == userInput).map(_._1)

              selectedSong match {
                case Some(_) => validAnswer = true
                case None => println("⚠ Invalid choice. Please choose a, b, c, or d.")
              }
            }

            totalQuestions += 1

            selectedSong.foreach { songTitle =>
              if (songTitle.equalsIgnoreCase(correctDisplay)) {
                println("✅ Correct!")
                correctAnswers += 1
              } else {
                println(s"❌ Incorrect. Correct answer was: $correctDisplay")
              }
            }

            questionCount += 1
            remainingQuestions -= 1

            if (remainingQuestions > 0) {
              var validContinueInput = false
              while (!validContinueInput) {
                val continueInput = readLineWithExit(s"\n❓ Do you want to continue? ($remainingQuestions questions left) (yes/no): ")
                if (continueInput.isEmpty) return
                if (continueInput == "yes") {
                  validContinueInput = true
                } else if (continueInput == "no") {
                  println("🔚 Ending the quiz early. Returning to main menu!")
                  continueQuiz = false
                  validContinueInput = true
                } else {
                  println("⚠ Please answer with 'yes' or 'no'.")
                }
              }
            } else {
              println("✅ That was the last question! Great job!")
            }
          }

        case None =>
          println("⚠ No lyrics found for that genre.")
      }

      println("\n🎉 Quiz complete!")
      
      // Improved analytics prompt with error handling
      var validAnalyticsResponse = false
      while (!validAnalyticsResponse) {
        val showAnalytics = readLineWithExit("Would you like to see your quiz analytics? (yes/no): ")
        if (showAnalytics.isEmpty) return
        
        showAnalytics.toLowerCase match {
          case "yes" | "y" =>
            analytics()
            validAnalyticsResponse = true
          case "no" | "n" =>
            println("🔙 Returning to the main menu.")
            validAnalyticsResponse = true
          case _ =>
            println("⚠ Please answer with 'yes' or 'no'.")
        }
      }
    }

    def analytics(): Unit = {
      if (!hadQuiz) {
        println("⚠ You haven't taken a quiz yet, so there's no analytics to show!")
        val startQuiz = readLineWithExit("🎤 Would you like to start a quiz now? (yes/no) ")
        if (startQuiz.isEmpty) return
        if (startQuiz.contains("yes")) {
          songQuiz()
        } else {
          println("🔙 Returning to the main menu.")
        }
      } else {
        println("📊 Quiz Analytics:")
        println(s"- Total Questions: $totalQuestions")
        println(s"- Correct Answers: $correctAnswers")
        if (totalQuestions > 0) {
          val percentage = (correctAnswers.toDouble / totalQuestions * 100).toInt
          println(s"- Accuracy: $percentage%")
        }
        println("🎤 Want to take another quiz, get a recommendation, or just chat more?")
      }
    }

    def showCapabilities(): Unit = {
      println(
        """🎵 Here's what I can do for you:

🎧 Music Recommendations - I can suggest songs based on your favorite genre and decade.
🎤 Music Quiz - Test your knowledge with a lyrics guessing game.
📊 Quiz Analytics - See your quiz performance stats (after taking at least one quiz).
💖 Cheer Up - Get a happy song recommendation when you're feeling down.
💬 Chat - Just want to talk? I'm here for that too!

Just say the word and I'll make it happen!""")
    }

    def handleBoredUser(): Unit = {
      println("😴 Sounds like you're bored! Here's what we can do:")
      showCapabilities()
    }

    def respondToUser(input: String): Unit = {
      if (checkForExit(input)) return

      val lowerInput = input.toLowerCase

      val sadnessKeywords = List(
        "sad", "not good", "bad", "upset", "depressed", "unhappy", "miserable", "feeling down",
        "lonely", "crying", "blue", "broken", "disappointed", "heartbroken", "gloomy", "hopeless"
      )

      val boredKeywords = List(
        "bored", "what else", "what do you have", "anything else", "i'm bored",
        "entertain me", "more please", "got anything new", "next", "change it",
        "i want something different", "give me another", "i'm tired of this", "this is dull"
      )

      val recommendKeywords = List(
        "recommend", "recomend", "suggest", "suggest a song", "recommend something", "playlist", "what should i listen to",
        "any suggestions", "play something", "got a song", "show me songs"
      )

      val quizKeywords = List(
        "start", "quiz", "game", "play a game", "begin quiz", "music quiz",
        "trivia", "start the game", "test me", "let's play", "quiz me",
        "launch quiz", "start the test"
      )

      val analyticsKeywords = List(
        "analytics", "score", "percentage", "my results", "show my score",
        "how did i do", "quiz score", "check results", "performance", "stats",
        "my analytics", "result", "quiz outcome"
      )

      val positiveKeywords = List(
        "good", "great", "fine", "awesome", "amazing", "fantastic", "cool",
        "i'm okay", "i'm good", "i'm happy", "nice", "i'm fine", "not bad",
        "doing well", "can't complain"
      )

      val exitKeywords = List(
        "bye", "exit", "quit", "goodbye", "see you", "later", "i'm done",
        "leave", "stop", "close", "end", "that's all", "peace out"
      )

      val noInterestKeywords = List(
        "no", "nah", "not interested", "don't want", "i'm good", "maybe later",
        "not now", "pass", "no thanks", "not feeling it", "skip", "i'll pass",
        "not into it", "don't feel like it"
      )

      val capabilitiesKeywords = List(
        "what can you do", "capabilities", "help", "options",
        "features", "what do you do", "how do you work",
        "what are your functions", "show commands"
      )

      val matchedCategories = List(
        if (sadnessKeywords.exists(lowerInput.contains)) Some("sadness") else None,
        if (boredKeywords.exists(lowerInput.contains)) Some("boredom") else None,
        if (recommendKeywords.exists(lowerInput.contains)) Some("recommend") else None,
        if (quizKeywords.exists(lowerInput.contains)) Some("quiz") else None,
        if (analyticsKeywords.exists(lowerInput.contains)) Some("analytics") else None,
        if (positiveKeywords.exists(lowerInput.contains)) Some("positive") else None,
        if (exitKeywords.exists(lowerInput.contains)) Some("exit") else None,
        if (noInterestKeywords.exists(lowerInput.contains)) Some("no_interest") else None,
        if (capabilitiesKeywords.exists(lowerInput.contains)) Some("capabilities") else None
      ).flatten

      if (matchedCategories.contains("quiz") && matchedCategories.contains("recommend")) {
        songQuiz()
        songRecommendationFlow()
        return
      }

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
          case "capabilities" =>
            showCapabilities()
        }
      }
    }

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