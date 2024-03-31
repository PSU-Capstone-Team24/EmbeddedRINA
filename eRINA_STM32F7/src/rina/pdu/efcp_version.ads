with Conversion; use Conversion;

generic
    type T is private;
package EFCP_Version is
    type EFCP_Version is new T and Getter_Setter;
end EFCP_Version;
