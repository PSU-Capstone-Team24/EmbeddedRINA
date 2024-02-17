generic
   type Queue_Index is mod <>;
   type T is private;

package Queues is

   type Queue_Array is array (Queue_Index) of T;

   protected type Queue is
      function Peek (L : Queue_Index) return T;
      function Size return Natural;
      function Empty return Boolean;
      function Full return Boolean;
      entry Push (V : T);
      entry Pop;
      entry Pop (V : out T);
   private
      N   : Natural     := 0;
      Idx : Queue_Index := Queue_Array'First;
      A   : Queue_Array;
      L   : Positive    := Positive (Queue_Index'Last);
   end Queue;

end Queues;
