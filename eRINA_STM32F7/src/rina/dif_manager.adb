package body DIF_Manager is

    Element_Not_Found : exception;

    function Get (Name : String; DIF_Type : DIF_Types) return DIF is
    begin
        for E of DIF_List loop
            if E.Name = Name and E.DIF_Type = DIF_Type then
                return E;
            end if;
        end loop;

        raise Element_Not_Found;
    end Get;

end DIF_Manager;