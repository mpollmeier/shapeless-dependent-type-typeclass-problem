import shapeless._

trait Converter[From] {
  type To
  def to(from: From): To
}

object Converter {
  type Aux[From, Out0] = Converter[From] { type To = Out0 }

  implicit val forInteger = new Converter[Integer] {
    type To = String
    def to(value: Integer) = value.toString
  }

  implicit val forHNil = new Converter[HNil] {
    type To = HNil
    def to(value: HNil) = HNil
  }

  implicit def forHList[Head, HeadConverted, Tail <: HList, TailConverted <: HList](
    implicit
    hConverter: Converter.Aux[Head, HeadConverted],
    tConverter: Converter.Aux[Tail, TailConverted]): Converter.Aux[Head :: Tail, HeadConverted :: TailConverted] =
    new Converter[Head :: Tail] {
      type To = HeadConverted :: TailConverted
      def to(values: Head :: Tail) = ???
    }
}

object Usage extends App {
  val single = the[Converter[Integer]]
  implicitly[single.To =:= String]
  val singleAux = the[Converter.Aux[Integer, String]]

  val hnil = the[Converter[HNil]]
  implicitly[hnil.To =:= HNil]
  val hnilAux = the[Converter.Aux[HNil, HNil]]

  val hlist = the[Converter[Integer :: HNil]]
  type expected = String :: HNil
  implicitly[hlist.To =:= expected]
  val hlistAux = the[Converter.Aux[Integer :: HNil, String :: HNil]]
}
