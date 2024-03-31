with Conversion; use Conversion;

generic
    type T is private;
package PDU_Type is
    type PDU_Type is new T and Getter_Setter;
end PDU_Type;
