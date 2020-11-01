-- PACKAGE MNESON.TEST (BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Mneson.Test is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   Total : Natural := 0;
   Failed : Natural := 0;

   procedure Inc (X : in out Integer) is begin X := X + 1; end;

   procedure Perform
     (Test : Proc; Name : String; Fatal : Boolean := False) is
   begin
      Inc (Total);
      Test.all;
   exception
      when E : others =>
         Inc (Failed);
         Put (Name & " failed");
         if Fatal then
            Put_Line (" (fatal)");
            Report;
            raise;
         else
            Put_Line (" (" & Exception_Information (E) & ")");
         end if;
   end;

   function Raises
     (Test : Proc; Error : Exception_Id) return Boolean is
   begin
      Test.all;
      return False;
   exception
      when E : others =>
         return Exception_Identity (E) = Error;
   end;

   procedure Assert
     (Test : Boolean; Name : String; Fatal : Boolean := False) is
   begin
      Inc (Total);
      if not Test then
         Inc (Failed);
         Put (Name & " failed");
         if Fatal then
            Put_Line (" (fatal)");
            Report;
            raise Program_Error;
         else
            New_Line;
         end if;
      end if;
   exception
      when E : others =>
         Inc (Failed);
         Put (Name & " failed");
         if Fatal then
            raise;
         else
            Put_Line (Exception_Information (E));
         end if;
   end;

   procedure Report is
   begin
      Put_Line (Integer'Image (Total) & " tests performed");
      Put_Line (Integer'Image (Failed) & " tests failed");
   end;

   procedure Time_Increase_Test
     (Number_Of_Chunks, Chunk_Size : Natural;
      Max_Inc_Factor, Max_Avg_Inc_Factor : Float;
      Process : Process_Chunk_Item;
      Test_Name : String := "")
   is
      use Ada.Calendar;
      Start_Time, End_Time, Start: Time;
      D, Prev_D : Duration;
      Avg_D : Duration := 0.0;
      Inc_Factor, Prev_Inc_Factor : Float;
      Avg_Inc_Factor : Float := 0.0;
      Prev_Failed : Natural := Failed;
   begin
      Put_Line
        ("TIME INCREASE TEST " & Test_Name);
      Put_Line
        (Natural'Image (Number_Of_Chunks) &
         " chunks of size" &
         Natural'Image (Chunk_Size));
      Put_Line
        ("max inc factor =" & Float'Image (Max_Inc_Factor));
      Put_Line
        ("max avg factor =" & Float'Image (Max_Avg_Inc_Factor));
      Start := Clock;
      for I in 1 .. Number_Of_Chunks loop
         Start_Time := Clock;
         for J in 1 .. Chunk_Size loop
            Process (I, J);
         end loop;
         End_Time := Clock;
         D := End_Time - Start_Time;
         Avg_D := Avg_D + D;
         if I > 1 then
            Inc_Factor := Float (D) / Float (Prev_D);
            Assert (Inc_Factor <= Max_Inc_Factor, "inc factor" & Float'Image (Inc_Factor) & " <= max");
            if I > 2 then
               Assert (Inc_Factor < Prev_Inc_Factor, "inc < prev at chunk" & I'Img);
            end if;
            Prev_Inc_Factor := Inc_Factor;
            Avg_Inc_Factor := Avg_Inc_Factor + Inc_Factor;
         end if;
         Prev_D := D;
      end loop;
      Avg_Inc_Factor := Avg_Inc_Factor / Float (Number_Of_Chunks - 1);
      Assert (Avg_Inc_Factor <= Max_Avg_Inc_Factor, "avg factor <= max");
      Put_Line
         ("(" & Natural'Image (Failed - Prev_Failed) & " errors)");
      Put_Line ("total time=" & Duration'Image (Clock - Start));
      Put_Line
         ("average increase factor =" & Float'Image (Avg_Inc_Factor));
      Put_Line
         ("average duration =" & Float'Image (Float (Avg_D) / Float (Number_Of_Chunks * Chunk_Size)));
      Put_Line
         ("END TIME INCREASE TEST");         
   end;

end;
