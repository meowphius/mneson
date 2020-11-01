-- PROCEDURE MNESON.TEST.RUN2 (BODY ONLY)
-- Chasing the problem of the poor absolute time of Get_Value
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Mneson.Calculus;
with Mneson.Structures;
with Mneson.Types;

procedure Mneson.Test.Run2 is

   package Calculus is new Mneson.Calculus (Base.Work);
   package Structures is new Mneson.Structures (Base.Work);

   use Calculus;
   use Mneson.Types;
   use Structures;

   ---------------------------
   -- CHUNK ITEM PROCESSORS --
   ---------------------------

   procedure Connection_Test (I, J : Positive) is
      Src1 : Vertex := To_Vertex ("s1" & I'Img);
      Src2 : Vertex := To_Vertex ("s2" & J'Img);
      Tgt : Vertex := To_Vertex ("tg" & I'Img & J 'Img);

      Sbj : Vertex := To_Vertex ("sbj" & I'Img);
      Att : Vertex := To_Vertex ("att" & J'Img);
      Atv : Vertex := To_Vertex ("atv" & I'Img & J 'Img);
   begin
      Connect (Src1, Tgt);
      Connect (Src2, Tgt);

      Connect (Sbj, New_Instance (Att, Atv));
   end;

   procedure Get_Value_Test (I, J : Positive) is
      Sbj : Vertex := To_Vertex ("sbj" & I'Img);
      Att : Vertex := To_Vertex ("att" & J'Img);
      Atv : Vertex := To_Vertex ("atv" & I'Img & J 'Img);
   begin
      Assert (Get_Value (Sbj, Att) = Atv, "sbj att = atv");
   end;

   procedure Common_Targets_Test (I, J : Positive) is
      Src1 : Vertex := To_Vertex ("s1" & I'Img);
      Src2 : Vertex := To_Vertex ("s2" & J'Img);
      Tgt : Vertex := To_Vertex ("tg" & I'Img & J 'Img);
      Virgin : Boolean := True;
      procedure Process (X : Vertex) is
      begin
         Assert (Virgin, "virgin");
         Assert (X = Tgt, "X = Tgt");
         Virgin := False;
      end;
   begin
      For_Each_Common_Target (Src1, Src2, Process'Unrestricted_Access);
   end;

   procedure Singleton_Test (I, J : Positive) is
      Src1 : Vertex := To_Vertex ("s1" & I'Img);
   begin
      Assert (Is_Valueless (Singleton (Src1)), "valueless singleton");
   end;

   procedure Extract_Singleton_Test (I, J : Positive) is
      Src1 : Vertex := To_Vertex ("s1" & I'Img);
   begin
      Assert (Extract (Singleton (Src1)) = Src1, "extract singleton");
   end;

   S : Selection;

   procedure Extract_S (I, J : Positive) is
   begin
      Assert (Extract (S) = To_Vertex ("S"), "extract S = S");
   end;

begin

   Create ("test");

   S := Singleton (To_Vertex ("S"));

   declare
      M : Natural := 0;
      N : Natural := 0;
      Max : Float := 2.0;
      Max_Avg : Float := 2.0;
   begin
      begin
         M := Natural'Value (Argument (1));
         N := Natural'Value (Argument (2));
         Max := Float'Value (Argument (3));
         Max_Avg := Float'Value (Argument (4));
      exception
         when Constraint_Error => M := 0; N := 0;
      end;
      Time_Increase_Test (M, N, Max, Max_Avg, Connection_Test'Unrestricted_Access, "connection");
      Time_Increase_Test (M, N, Max, Max_Avg, Get_Value_Test'Unrestricted_Access, "get value");
--      Time_Increase_Test (M, N, Max, Max_Avg, Common_Targets_Test'Unrestricted_Access, "common targets");
--      Time_Increase_Test (M, N, Max, Max_Avg, Singleton_Test'Unrestricted_Access, "singleton");
--      Time_Increase_Test (M, N, Max, Max_Avg, Extract_S'Unrestricted_Access, "extract S");
--      Time_Increase_Test (M, N, Max, Max_Avg, Extract_Singleton_Test'Unrestricted_Access, "extract singleton");
   end;

   Perform (Close'Unrestricted_Access, "close");
   Report;
end;
