package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   * 它在做的事：來你給我一個整數，我跟你說他有沒有在這個set裏
   */
  def singletonSet(elem: Int): FunSet = (x:Int ) => x==elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   * 拆解：
   * 1.def union: 我現在定一個“聯集”的定義def
   * 2.(s,t)餵兩個變數進去，兩個變數都是FunSet
   * 3. :FunSet 這個def會回傳funset;上去找funset，啊，最上面type說它讀一個整數，給一個boolean的結果
   * 4.(x:(Int))現在我讓funset的方法讀入整數，這個整數的變數就叫它x吧
   * 5.s(x)||t(x)他拿到這個x之後，會分別餵到s跟t裏面；回去看一下funset是“讀整數給boolean”，所以分別會有s跟t兩個boolean的結果
   * ||部分：這兩個boolean的結果可以用XOR合併成一個boolean，所以最後結果是一個boolean，有符合funset說"給一個boolean的結果"，給過
   */
  def union(s: FunSet, t: FunSet): FunSet = (x:(Int))=> s(x)||t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (x:(Int))=>s(x)&&t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = (x:(Int)) =>s(x) && (!t(x))

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = (x:(Int))=> s(x)&&p(x)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    def iter(a: Int): Boolean =
      if (a>bound) then true
      else if (contains(s,a)) && !p(a) then false
      else iter(a+1)
    iter(-bound)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s, (x => !p(x)))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = (x =>exists(s, y=>f(y)==x))

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

object FunSets extends FunSets
