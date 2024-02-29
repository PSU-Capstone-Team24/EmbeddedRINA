package body Queues is

   protected body Queue is

      --  Queues shouldn't be iterable? Yeah I don't really care
      function Peek (L : Queue_Index) return T is
      begin
         return A (L + (X mod Queue_Index'Last));
      end Peek;

      function Size return Natural is
      begin
         return N;
      end Size;

      function Empty return Boolean is (N = 0);

      function Full return Boolean is (N = A'Length);

      entry Push (V : T) when N <= L is
      begin
         A (Idx) := V;

         Idx := Idx + 1;
         N   := N + 1;
      end Push;

      entry Pop (V : out T) when N > 0 is
      begin
         N := N - 1;
         V := A (Idx - Queue_Index (N) - 1);
         X := X + 1;
      end Pop;

      entry Pop when N > 0 is
      begin
         N := N - 1;
         X := X + 1;
      end Pop;

   end Queue;

end Queues;
