-- PACKAGE MNESON.TEST (SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Exceptions;
with Mneson.Base;

package Mneson.Test is

   package Base is new Mneson.Base;

   use Ada.Exceptions;
   use Base;

   type Proc is access procedure;

   procedure Assert
     (Test : Boolean; Name : String; Fatal : Boolean := False);

   procedure Perform
     (Test : Proc; Name : String; Fatal : Boolean := False);

   function Raises
     (Test : Proc; Error : Exception_Id) return Boolean;

   type Process_Chunk_Item is access procedure (I, J : Positive);

   procedure Time_Increase_Test
     (Number_Of_Chunks, Chunk_Size : Natural;
      Max_Inc_Factor, Max_Avg_Inc_Factor : Float;
      Process : Process_Chunk_Item;
      Test_Name : String := "");

   procedure Report;

end;
