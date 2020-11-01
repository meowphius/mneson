-- PACKAGE MNESON.TEMPLATES (SPEC ONLY)
-- Support for the formal package idiom.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Mneson.Types;

package Mneson.Templates is

   use Mneson.Types;

   generic
      with procedure Create (Name : String) is <>;
      with procedure Open (Name : String) is <>;
      with procedure Save is <>;
      with procedure Save_As (Name : String) is <>;
      with procedure Close is <>;
      with procedure Set_Monitor (Monitor : Process_String) is <>;
      with procedure Set_Logging (On : Boolean) is <>;
   package File is end;

   generic
      type Vertex_Array is array (Positive_64 range <>) of Vertex;
      --with package Order is new Mneson.Order (Vertex);
      with function To_Vertex (Value : String) return Vertex is <>;
      with function To_Vertex (Value : Integer_64) return Vertex is <>;
      with function Valueless_Vertex
        (Number : Serial_Number) return Vertex is <>;
      with function New_Serial_Number return Serial_Number is <>;
      with function New_Vertex return Vertex is <>;
      with function Value (X : Vertex) return Integer_64 is <>;
      with function Value (X : Vertex) return String is <>;
      with function Length (X : Vertex) return Natural is <>;
      with function Slice
        (X : Vertex; Low : Positive; High : Natural) return String is <>;
      with procedure Connect (Source, Target : Vertex) is <>;
      with procedure Disconnect (Source, Target : Vertex) is <>;
      with procedure Disconnect_From_Targets (Source : Vertex) is <>;
      with procedure Disconnect_From_Sources (Target : Vertex) is <>;
      with procedure Disconnect (X : Vertex) is <>;
      with procedure Reconnect
        (Source, Target, New_Target : Vertex) is <>;
      with procedure Inv_Reconnect
        (Target, Source, New_Source : Vertex) is <>;
      with function Connected
        (Source, Target : Vertex) return Boolean is <>;
      with function Connected (Source : Vertex) return Boolean is <>;
      with function Inv_Connected (Target : Vertex) return Boolean is <>;
      with procedure For_Each_Target
        (Source : Vertex; Process : Process_Vertex) is <>;
      with procedure For_Each_Source
        (Target : Vertex; Process : Process_Vertex) is <>;
      with procedure For_Each_Link (Process : Process_Link) is <>;
      with procedure For_Each_Common_Target
        (Source_1, Source_2 : Vertex; Process : Process_Vertex) is <>;
      with function Is_Integer_64 (X : Vertex) return Boolean is <>;
      with function Is_String (X : Vertex) return Boolean is <>;
      with function Is_Valueless (X : Vertex) return Boolean is <>;
      with function Img (X : Vertex) return String is <>;
      with function Val (Img : String) return Vertex is <>;
   package Work is end;

end;
