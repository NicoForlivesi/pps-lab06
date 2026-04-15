package it.unibo.pps.ex2

import it.unibo.pps.ex1

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 *
 */
trait ConferenceReviewing:
  /**
   * For each article, the reviewer has to reply to all the following questions
   */
  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

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
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl()

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: List[(Int, Map[Question, Int])] = List.empty
  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews :+ (article, scores)

  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    reviews = reviews :+ (article, Map(
      Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significance,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin
    ))

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(_._1 == article).map(_._2(question)).sortWith(_>_)

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  override def averageFinalScore(article: Int): Double = ???

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  override def acceptedArticles: Set[Int] = ???

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  override def sortedAcceptedArticles: List[(Int, Double)] = ???

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  override def averageWeightedFinalScoreMap: Map[Int, Double] = ???
