-- PACKAGE MNESON.TYPES (SPEC ONLY)
-- Basic types, including Vertex.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

package Mneson.Types is

   type Integer_64 is range - 2 ** 63 + 1 .. 2 ** 63 - 1;
   for Integer_64'Size use 64;

   subtype Natural_64 is Integer_64 range 0 .. Integer_64'Last;
   subtype Positive_64 is Natural_64 range 1 .. Natural_64'Last;

   type Natural_16 is range 0 .. 2 ** 16 - 1;
   for Natural_16'Size use 16;

   type Modular_64 is mod 2 ** 64;
   for Modular_64'Size use 64;

   type Vertex is record
      Tip : Natural_16;
      Cue : Modular_64;
   end record;
   for Vertex'Size use 80;
   pragma Pack (Vertex);

   subtype Vertex_Count is Natural_64;
   subtype Serial_Number is Vertex_Count range 1 .. Vertex_Count'Last;

   type Process_Vertex is access procedure (X : Vertex);
   type Process_Link is access procedure (Source, Target : Vertex);
   type Process_String is access procedure (S : String);

   Not_Implemented_Yet : exception;
   State_Error         : exception;
   Syntax_Error        : exception;
   Type_Error          : exception;

end;

-- NOTES

-- The term "Modular" is used instead of the commonly used "Unsigned",
-- because the latter is misleading, as the type can represent
-- signed values.

-- Type Vertex is exposed (and not private) for several reasons,
-- including the full view being required for container package
-- instantiations.

-- Integer_64 starts at one plus the usual bound for reasons
-- including the possibility that this might be more adequate for
-- some implementations.

-- A type Vertex_Array is defined in Mneson.Base (and not here)
-- for reasons explained there.