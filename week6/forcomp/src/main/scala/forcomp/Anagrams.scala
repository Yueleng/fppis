package forcomp

import scala.annotation.tailrec

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    count((w.toLowerCase.toList groupBy (c=>c)).toList) sortBy (_._1)
    // map((character: Char, listOfC: List[Char]): (Char, Int) => (character, listOfC.size))
  }

  def wordOccurrencesAlt(w: Word): Occurrences = {
    (w.toSeq groupBy (c => c.toLower) map {case (x, y) => (x, y.length)}).toList.sorted
  }

  // Helper Function
  def count(occ: List[(Char, List[Char])]): Occurrences = {
    def countAcc(occ: List[(Char, List[Char])], acc: List[(Char, Int)]): Occurrences = {
      if (occ.isEmpty) acc
      else countAcc(occ.tail, (occ.head._1, occ.head._2.size) :: acc)
    }
    countAcc(occ, List())
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    s map (word => word forall ( char => char.isLetter))

    @tailrec
    def oneWord(s: Sentence, w: Word): Word = {
      if (s.isEmpty) w
      else oneWord(s.tail, s.head.concat(w))
    }

    wordOccurrences(oneWord(s, ""))
  }

  def sentenceOccurrencesAlt(s: Sentence): Occurrences = {
    if (s.isEmpty) Nil
    else s map wordOccurrences reduce mergedOccurrences
  }

  def mergedOccurrences(lhs: Occurrences, rhs: Occurrences): Occurrences = {
    def merge(lhs: Occurrences, rhs: Occurrences, accl: Occurrences): Occurrences = (lhs, rhs) match {
      case (Nil, Nil) => accl.reverse
      case (_, Nil) => accl.reverse ::: lhs
      case (Nil, _) => accl.reverse ::: rhs
      case (x :: xs, y :: ys) =>
        if (x._1 < y._1) merge (lhs.tail, rhs, x :: accl)
        else if (x._1 > y._1) merge (lhs, rhs.tail, y :: accl)
        else merge(lhs.tail, rhs.tail, (x._1, x._2 + y._2) :: accl)
    }
    merge(lhs, rhs, List())
  }



  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences withDefaultValue List()

  lazy val dictionaryByOccurrencesAlt: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  def wordAnagrmasAlt(word: Word): List[Word] = {
    val occurrences = wordOccurrences(word)
    dictionaryByOccurrences.getOrElse(occurrences, Nil)
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    @tailrec
    def combineIter(occurrences: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
      if (occurrences.isEmpty) acc
      else {
        val charOcc = occurrences.head
        val c = charOcc._1
        val nextIterAcc = (for {
          i <- 0 to charOcc._2
          charsOcc <- if (acc.isEmpty) List(List()) else acc
        } yield ((c, i) :: charsOcc) sortBy (_._1) ).toList
        combineIter(occurrences.tail, nextIterAcc)
      }
    }
    combineIter(occurrences, List(List())) map (occ => occ filter (charOcc => charOcc._2 > 0))
  }

  def combinationsAlt(occurrences: Occurrences): List[Occurrences] = {
    val s = occurrencesToString(occurrences)
    val subseqs = subSequences(s).toSet // .toSet is useless here, we can get rid of toSet and toList in the next line.
    subseqs.map(wordOccurrences).toList
  }

  // Helper Function
  def occurrencesToString(occurrences: Occurrences): String = {
    occurrences.foldLeft("")((s,e) => s + e._1.toString * e._2)
  }

  def subSequences(s: String): List[String] = {
    def combinations(s: String, accl: List[String]): List[String] = {
      if (s.isEmpty) accl
      else {
        val lists = accl.map(_ + s.head)
        combinations(s.tail, lists ::: accl)
      }
    }
    val empty = List("")
    combinations(s, empty)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    @tailrec
    def getCount(occ: Occurrences, c: Char): Int = {
      if (occ.isEmpty) 0
      else if (occ.head._1 == c) occ.head._2
      else getCount(occ.tail, c)
    }

//    def adjust(pair: (Char, Int)): (Char, Int) = {
//      val (c, count) = pair
//      println(c + ": " + count)
//      println(c + ": " +  getCount(x, c))
//      c -> (getCount(x, c) - count)
//    }

    def deductTerm(terms0: Map[Char, Int], term: (Char, Int)): Map[Char, Int] = {
      // term is from x, terms is y, weird!!
      val terms = terms0 withDefaultValue 0 // immutability, thus we have to create a new var
      val (c, count) = term
      terms + (c -> (-terms(c) + count)) // Map + (Key -> Value): Means update or add new key value pair.
    }

    if (y.isEmpty) x filter (pair => pair._2 > 0) sortBy (_._1)
    else (x.toMap foldLeft y.toMap)(deductTerm).toList filter (pair => pair._2 > 0) sortBy (_._1)

  }

  def subtractAlt(x: Occurrences, y: Occurrences): Occurrences = {
    val xmap = x.toMap
    y.foldLeft(xmap) {
      case (map, (char, frequency)) => {
        val remainder = map(char) - frequency
        if (remainder == 0) map - char // delete this entry : (char: charCount) in map
        else map + (char -> remainder)
      }
    }.toList.sorted
  }



  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  // My solution: Concise. :-)
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    // List[(Char, Int)]
    val sentenceOcc = sentenceOccurrences(sentence)


    def sentenceOccAnagramsIter(sentenceOcc: Occurrences): List[Sentence] = {
      // combination: List[List[(Char,Int)]]
      if (sentenceOcc.isEmpty) List(List())
      else {
        val comb = combinations(sentenceOcc)

        for {
          subset <- comb
          word <- dictionaryByOccurrences(subset)
          rest <- sentenceOccAnagramsIter(subtract(sentenceOcc, subset))
        } yield word :: rest
      }
    }
    sentenceOccAnagramsIter(sentenceOcc)
  }

  // Alternative answer by others: Tedious! :-(
  def sentenceAnagramsAlt(sentence: Sentence): List[Sentence] = {
    val senOcc = sentenceOccurrences(sentence)
    val occurrences = occurrencesCombinations(senOcc) // every element in occurrences represents a group of sentences
    val sentences = occurrences flatMap { occurrence => { // occurrence 会扩容成多个(will be expanded to multiple) sentences 最后用 (and will finally use) flatMap 摊开 (to spread out)
      // occurrence = List(List(('s', 1), ('a', 1), ('e', 1), ('e', 1)), List(('m', 1), ('y', 1)))  => sentences = List(List("Sean", "sane"), List("my"))
      val groupOfSentences = occurrence.map(dictionaryByOccurrences)
      elementCombinations(groupOfSentences)
      }
    }
    sentences
  }

  def occurrencesCombinations(occurrences: Occurrences): List[List[Occurrences]] = {
    if (occurrences.isEmpty) List(List())
    else for {
      left <- combinations(occurrences).filter(dictionaryByOccurrences.contains)
      remainder = subtract(occurrences, left)
      right <- occurrencesCombinations(remainder)
      if sumsEqual(remainder, right)
    } yield left :: right
  }

  def sumsEqual(occurrences: Occurrences, ol: List[Occurrences]): Boolean = {
    if (ol.isEmpty) occurrences.isEmpty
    else ol.reduce(mergedOccurrences) == occurrences
  }

  // This function is for constructing individual sentences from a group of sentences
  // lists = List(List("Sean", "sane"), List("my")) => return: List(List("Sean", "my"), List("sane", "my"))
  def elementCombinations(lists: List[List[String]]): List[Sentence] = {
    if (lists.isEmpty) List(List())
    else elementCombinations(lists.tail).flatMap(xs => lists.head.map(_ :: xs))
  }




}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}


