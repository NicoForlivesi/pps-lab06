package it.unibo.pps.ex2

import it.unibo.pps.ex2.ConferenceReviewing.Question.{CONFIDENCE, FINAL}

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 *
 */
trait ConferenceReviewing:
  import ConferenceReviewing.Question
  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

  def apply(): ConferenceReviewing = new ConferenceReviewingImpl()

class ConferenceReviewingImpl extends ConferenceReviewing:
  import ConferenceReviewing.Question
  private var reviews: List[(Int, Map[Question, Int])] = List.empty

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews :+ (article, scores)

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    reviews = reviews :+ (article, Map(
      Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significance,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin
    ))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(_._1 == article).map(_._2(question)).sortWith(_<_)

  override def averageFinalScore(article: Int): Double =
    val finalScores = orderedScores(article, Question.FINAL)
    finalScores.sum.toDouble / finalScores.size

  override def acceptedArticles: Set[Int] =
    def _checkingRel(articles: List[Int]): List[Int] = articles match
      case h :: t =>
        val filteredRevs = reviews.filter(_._1 == h).filter(_._2(Question.RELEVANCE) >= 8)
        if filteredRevs.isEmpty then _checkingRel(t) else h :: _checkingRel(t)
      case _ => Nil
    _checkingRel(reviews.map(_._1).filter(averageFinalScore(_) > 5)).toSet

  override def sortedAcceptedArticles: List[(Int, Double)] =
    acceptedArticles.map(a => (a, averageFinalScore(a))).toList.sortWith(_._2 < _._2)

  override def averageWeightedFinalScoreMap: Map[Int, Double] =
    def _calculateAvgWeightedFin(article: Int): Double =
      val finWeightedRevs =
        reviews.filter(_._1 == article).map(c => c._2(Question.CONFIDENCE) * c._2(Question.FINAL) / 10.0)
      finWeightedRevs.sum / finWeightedRevs.size
    reviews.map(c => c._1 -> _calculateAvgWeightedFin(c._1)).toMap

object Test extends App:
  import ConferenceReviewing.Question
  private val cr = ConferenceReviewing()

  cr.loadReview(1, 8, 8, 6, 8)
  cr.loadReview(1, 9, 9, 6, 9)
  cr.loadReview(2, 9, 9, 10, 9)
  cr.loadReview(2, 4, 6, 10, 6)
  cr.loadReview(3, 3, 3, 3, 3)
  cr.loadReview(3, 4, 4, 4, 4)
  cr.loadReview(4, 6, 6, 6, 6)
  cr.loadReview(4, 7, 7, 8, 7)
  
  println(cr.orderedScores(1, Question.FINAL))       // List(8, 9)
  println(cr.orderedScores(2, Question.RELEVANCE))   // List(4, 9)
  println(cr.orderedScores(3, Question.CONFIDENCE))  // List(3, 4)

  println(cr.averageFinalScore(1))  // 8.5
  println(cr.averageFinalScore(2))  // 7.5
  println(cr.averageFinalScore(3))  // 3.5
  println(cr.averageFinalScore(4))  // 6.5

  println(cr.acceptedArticles)  // Set(1, 2)

  println(cr.sortedAcceptedArticles)  // List((2, 7.5), (1, 8.5))

  println(cr.averageWeightedFinalScoreMap) // Map(1 -> 5.1, 2 -> 7.5, 3 -> 1.25, 4 -> 4.6)