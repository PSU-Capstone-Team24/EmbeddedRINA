generic
    type T is private;
package Conversion is
    -- interface for all generics to implement for getting and setting record elements
    type Decoder is interface;
    function Decode
       (A : in out Decoder; E : Natural'Class)
        return T is abstract; -- used inside Getters

    type Encoder is interface;
    function Encode
       (A : in out Encoder; E : T)
        return Natural'Class is abstract; -- used by Setters

    -- interface for all getter setters
    type Getter_Setter is interface and Encoder and Decoder;
end Conversion;
