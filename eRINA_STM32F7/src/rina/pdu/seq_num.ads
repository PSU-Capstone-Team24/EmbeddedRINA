with Conversion; use Conversion;

generic
    type T is private;
package Seq_Num is
    type Seq_Num is new T and Getter_Setter;
end Seq_Num;
