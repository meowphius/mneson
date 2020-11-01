-- GENERIC PACKAGE MNESON.BASE (SPEC)
-- An instantiation of this package represents an untyped graph.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with AI302.Containers;
with AI302.Containers.Sorted_Sets;
with AI302.Containers.String_Hashed_Maps;
with AI302.Containers.Vectors;
with Mneson.Order;
with Mneson.Templates;
with Mneson.Types;

generic package Mneson.Base is

   use Mneson.Types;

   procedure Create (Name : String);
   procedure Open (Name : String);
   procedure Save;
   procedure Save_As (Name : String);
   procedure Close;
   procedure Set_Monitor (Monitor : Process_String);
   procedure Set_Logging (On : Boolean);

   function To_Vertex (Value : String) return Vertex;
   function To_Vertex (Value : Integer_64) return Vertex;
   function Valueless_Vertex (Number : Serial_Number) return Vertex;
   function New_Serial_Number return Serial_Number;
   function New_Vertex return Vertex;

   function "<" (Left, Right : Vertex) return Boolean;
   package Vertex_Order is new Mneson.Order (Vertex);
   use Vertex_Order;
   subtype Vertex_Array is Vertex_Order.One_Dimensional_Array;

   function Value (X : Vertex) return Integer_64;
   function Value (X : Vertex) return String;
   function Length (X : Vertex) return Natural;
   function Slice
     (X : Vertex; Low : Positive; High : Natural) return String;

   procedure Connect (Source, Target : Vertex);
   procedure Disconnect (Source, Target : Vertex);
   procedure Disconnect_From_Targets (Source : Vertex);
   procedure Disconnect_From_Sources (Target : Vertex);
   procedure Disconnect (X : Vertex);
   procedure Reconnect (Source, Target, New_Target : Vertex);
   procedure Inv_Reconnect (Target, Source, New_Source : Vertex);
   function Connected (Source, Target : Vertex) return Boolean;
   function Connected (Source : Vertex) return Boolean;
   function Inv_Connected (Target : Vertex) return Boolean;

   procedure For_Each_Target (Source : Vertex; Process : Process_Vertex);
   procedure For_Each_Source (Target : Vertex; Process : Process_Vertex);
   procedure For_Each_Link (Process : Process_Link);

   procedure For_Each_Common_Target
     (Source_1, Source_2 : Vertex; Process : Process_Vertex);

   function Is_Integer_64 (X : Vertex) return Boolean;
   function Is_String (X : Vertex) return Boolean;
   function Is_Valueless (X : Vertex) return Boolean;
   function Img (X : Vertex) return String;
   function Val (Img : String) return Vertex;

   package File is new Mneson.Templates.File;
   package Work is new Mneson.Templates.Work (Vertex_Array);

private

   use AI302.Containers;
   subtype Link_Type is Vertex_Array (1 .. 2);
   package Link_Sets is new Sorted_Sets (Link_Type);
   package String_Maps is new String_Hashed_Maps (Vertex);
   package Vertex_Vectors is new Vectors (Positive_64, Vertex);

end;

-- NOTES

-- Type Vertex_Array is defined here (and not in Mneson.Types)
-- because the order relations depend on "<" for Vertex,
-- and the latter depends on the graph state (for string vertices).

-- Inv = inverse, inversely.

-- Instantiations of AI302 container packages must be done here
-- (and not in the body), for a number of reasons.
-- Of necessity, some required parameter entities are also declared here.

-- Some entities do not depend on the state of the instantiation,
-- e.g. To_Vertex (Value : Integer_64), Is_Integer_64, Is_String,
-- but they are put here not to complicate the package system,
-- and also because separating the dependent from the independent entities
-- would separate To_Vertex (Value : Integer_64) (independent) from
-- To_Vertex (Value : String) (dependent), which would be unnatural.