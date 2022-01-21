package kofre

import kofre.Lattice.merge

import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A]:
  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case
    * that left >> right, i.e., that left is the current state and right the delta
    */
  def merge(left: A, right: A): A

object Lattice {
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A): A       = apply[A].merge(left, right)

  // extension [A: Lattice](left: A)
  //  @targetName("mergeSyntax")
  //  def merge(right: A): A = Lattice.merge(left, right)

  // this seems to have better 2.13 compatibility
  implicit class Operators[A: Lattice](left: A):
    def merge(right: A): A = Lattice.merge(left, right)

  // /////////////// common instances below ///////////////

  given setLattice[A]: Lattice[Set[A]] = _ union _

  given optionLattice[A: Lattice]: Lattice[Option[A]] =
    case (None, r)          => r
    case (l, None)          => l
    case (Some(l), Some(r)) => Some(Lattice.merge[A](l, r))

  given mapLattice[K, V: Lattice]: Lattice[Map[K, V]] =
    (left, right) =>
      left.to(HashMap).merged(right.to(HashMap)) {
        case ((id, v1), (_, v2)) => (id, (v1 merge v2))
      }

  inline def derived[T <: Product](using m: Mirror.ProductOf[T]): Lattice[T] =
    val lattices = LatticeDeriveImpl.summonList[m.MirroredElemTypes]
    // convert to array to make lookup during merge fast
    LatticeDeriveImpl.mergeProduct(m, lattices.toArray.asInstanceOf[Array[Lattice[Any]]])
}

object LatticeDeriveImpl {

  def mergeProduct[T <: Product](p: Mirror.ProductOf[T], lattices: Seq[Lattice[Any]]): Lattice[T] =
    new Lattice[T]:
      def merge(left: T, right: T): T =
        p.fromProduct(new Product {
          def canEqual(that: Any): Boolean = left.canEqual(that)
          def productArity: Int            = right.productArity
          def productElement(i: Int): Any  = lattices(i).merge(left.productElement(i), right.productElement(i))
        })

  inline def summonList[T <: Tuple]: List[Lattice[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Lattice[t]] :: summonList[ts]
}
