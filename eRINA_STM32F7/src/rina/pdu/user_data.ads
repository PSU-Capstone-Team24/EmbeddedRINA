with Conversion; use Conversion;

generic
    type T is private;
package User_Data is
    type User_Data is new T and Getter_Setter;
end User_Data;
