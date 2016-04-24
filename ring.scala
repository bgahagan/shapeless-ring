package ring {

  import shapeless._
  import record._
  import ops.hlist.{ ZipWithKeys }
  import ops.record.{ Keys, Values, SelectAll, Merger }

  @annotation.implicitNotFound(msg = "Not all field for ${CC} in record ${R}")
  trait NarrowToCaseClass[R <: HList, CC] extends DepFn1[R] with Serializable { type Out = CC }
  object NarrowToCaseClass {
    def apply[R <: HList, CC](implicit ntcc: NarrowToCaseClass[R, CC]) = ntcc
    
    implicit def ntcc[R <: HList, K <: HList, V <: HList, T <: HList, CC](
      implicit gen: LabelledGeneric.Aux[CC, T],
      keys: Keys.Aux[T, K],
      selectAll: SelectAll.Aux[R, K, V],
      withKeys: ZipWithKeys.Aux[K, V, T]): NarrowToCaseClass[R, CC] = 
      new NarrowToCaseClass[R, CC] {
        def apply(rec: R): CC = {
          val vals = selectAll(rec)
          val newRec = vals.zipWithKeys(keys())
          gen.from(newRec)
        }
      }
  }

  @annotation.implicitNotFound(msg = "Not all field for ${A} in record ${Req}")
  trait Handler[A, B, Req <: HList] extends DepFn2[Req, A => B] with Serializable { 
    type Out <: HList
  }
  object Handler {
    type Aux[A, B, Req <: HList, Rsp <: HList] = Handler[A, B, Req] { type Out = Rsp }
    def apply[A, B, Req <: HList](implicit hl: Handler[A, B, Req]): Aux[A, B, Req, hl.Out] = hl

    implicit def handler[A, B, Req <: HList, Rsp <: HList](
      implicit 
      genB: LabelledGeneric.Aux[B, Rsp],
      narrow: NarrowToCaseClass[Req, A]): Aux[A, B, Req, genB.Repr] = 
      new Handler[A, B, Req] {
        type Out = genB.Repr
        def apply(req: Req, f: A => B): Out = {
          val a : A = narrow(req)
          val b: B = f(a)
          genB.to(b)
        }
      }
  }

  @annotation.implicitNotFound(msg = "Not all field for ${A} in record ${In}")
  trait TransformWithMerge[A, B, In <: HList] extends DepFn2[In, A => B] with Serializable { type Out <: HList }
  object TransformWithMerge {
    type Aux[A, B, In <: HList, Out0 <: HList] = TransformWithMerge[A, B, In] { type Out = Out0 }
    def apply[A, B, In <: HList](implicit twm: TransformWithMerge[A, B, In]): Aux[A, B, In, twm.Out] = twm

    implicit def transformWithMerge[A, B, In <: HList, Out0 <: HList](
      implicit 
      genB: LabelledGeneric.Aux[B, Out0],
      narrow: NarrowToCaseClass[In, A],
      merger: Merger[In, Out0]): Aux[A, B, In, merger.Out] = 
      new TransformWithMerge[A, B, In] {
        type Out = merger.Out
        def apply(in: In, f: A => B): Out = {
          val a : A = narrow(in)
          val b: B = f(a)
          in.merge(genB.to(b))
        }
      }
  }

  trait Chain[In] extends DepFn1[In] with Serializable { type Out }
  object Chain {
    type Aux[In, Out0] = Chain[In] { type Out = Out0 }
    def apply[T](
      implicit gen: LabelledGeneric[T]
      ): Chain.Aux[gen.Repr, gen.Repr] = 
        new Chain[gen.Repr] {
          type Out = gen.Repr
          def apply(in: gen.Repr): gen.Repr = in
        }

    implicit class ChainOps[In <: HList, Out <: HList](c: Chain.Aux[In, Out]) {
      def transform[A, B, NewOut <: HList](f: A => B)(
        implicit twm: TransformWithMerge.Aux[A, B, Out, NewOut]): Chain.Aux[In, twm.Out] = 
          new Chain[In] {
            type Out = twm.Out
            def apply(in: In): twm.Out =
              twm(c(in), f)
          }

      def handle[A, B, Rsp <: HList](f: A => B)(
        implicit handler: Handler.Aux[A, B, Out, Rsp]): Chain.Aux[In, handler.Out] = 
          new Chain[In] {
            type Out = handler.Out
            def apply(in: In): handler.Out =
              handler(c(in), f)
          }

      def build[T](
        implicit gen: LabelledGeneric.Aux[T, In]): Chain.Aux[T, Out] = 
          new Chain[T] {
            type Out = c.Out
            def apply(in: T): Out =
              c(gen.to(in))
          }
    }
  }

}
