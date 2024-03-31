with Conversion; use Conversion;

generic
   type T is private;
package PDU_Flags is
   type PDU_Flags is new T and Getter_Setter;
end PDU_Flags;
