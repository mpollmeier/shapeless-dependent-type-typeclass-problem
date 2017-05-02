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

  /* first attempt: doesn't work because compiler doesn't find that TailConverted is an HList:
   * type arguments [HeadConverted,TailConverted] do not conform to class ::'s type parameter bounds [+H,+T <: shapeless.HList]
   * [error]       type To = HeadConverted :: TailConverted
   */
  // implicit def forHList[Head, HeadConverted, Tail <: HList, TailConverted](
  //   implicit
  //   hConverter: Converter.Aux[Head, HeadConverted],
  //   tConverter: Converter.Aux[Tail, TailConverted]): Converter[Head :: Tail] =
  //   new Converter[Head :: Tail] {
  //     type To = HeadConverted :: TailConverted
  //     def to(values: Head :: Tail) = ???
  //   }

  /* second attempt: compiles, and finds `the[Converter[Integer :: HNil]]`, but doesn't find `the[Converter.Aux[Integer :: HNil, String :: HNil]]`
   */
  implicit def forHList[Head, HeadConverted, Tail <: HList, TailConverted, TailConvertedHList <: HList](
    implicit
    hConverter: Converter.Aux[Head, HeadConverted],
    tConverter: Converter.Aux[Tail, TailConverted],
    isHList: IsHList.Aux[TailConverted, TailConvertedHList]
    ): Converter[Head :: Tail] = new Converter[Head :: Tail] {
      type To = HeadConverted :: TailConvertedHList
      def to(values: Head :: Tail) = ???
    }
}

trait IsHList[L] {
  type Out
}
object IsHList {
  type Aux[L, Out0] = IsHList[L] { type Out = Out0 }
  implicit def isHList[L <: HList] = new IsHList[L] { type Out = L }
}

object Usage extends App {
  val single = the[Converter[Integer]]
  implicitly[single.To =:= String]
  val singleAux = the[Converter.Aux[Integer, String]]

  val hnil = the[Converter[HNil]]
  implicitly[hnil.To =:= HNil]
  val hnilAux = the[Converter.Aux[HNil, HNil]]

  val hlist = the[Converter[Integer :: HNil]]
  // implicitly[hlist.To =:= String] // fails, but why?
  // val hlistAux = the[Converter.Aux[Integer :: HNil, String :: HNil]] // that's what I want to get, but doesn't find implicit

  object explicitly {
    // summoning all the implicits works
    val hConverter = the[Converter.Aux[Integer, String]]
    implicitly[hConverter.To =:= String]
    val tConverter = the[Converter.Aux[HNil, HNil]]
    implicitly[tConverter.To =:= HNil]
    // val tailIsHList = the[tConverter.To <:< HList]
    val tailIsHList = the[IsHList[tConverter.To]]


    // but when passing them to the converter, the compiler doesn't infer the return type as expected...
    // val wanted: Converter.Aux[Integer :: HNil, String :: HNil] = Converter.forHList(hConverter, tConverter, tailIsHList2)
    // [error]  found   : Converter[Integer :: shapeless.HNil]
    // [error]  required: Converter.Aux[Integer :: shapeless.HNil,String :: shapeless.HNil]
    // [error]     (which expands to)  Converter[Integer :: shapeless.HNil]{type To = String :: shapeless.HNil}
    // [error]     val wanted: Converter.Aux[Integer :: HNil, String :: HNil] = Converter.forHList(hConverter, tConverter, tailIsHList2)
  }
}
