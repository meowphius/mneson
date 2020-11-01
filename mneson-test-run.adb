-- PROCEDURE MNESON.TEST.RUN (BODY ONLY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Mneson.Calculus;
with Mneson.Structures;
with Mneson.Types;

procedure Mneson.Test.Run is

   package Calculus is new Mneson.Calculus (Base.Work);
   package Structures is new Mneson.Structures (Base.Work);

   use Calculus;
   use Mneson.Types;
   use Structures;

   Top, Branch1, Branch2, Source1, Target1,
      Rec1, Instance11, Instance12, Set1, Set2,
      Elm1, Elm12, Elm2 : Vertex;

   procedure Populate is
   begin
      Top := New_Vertex;
      Branch1 := New_Vertex;
      Branch2 := New_Vertex;
      Connect (Top, Branch1);
      Connect (Top, Branch2);
      Instance11 := New_Instance ("at11", "av11");
      Instance12 := New_Instance ("at12", "av12");
      Rec1 := New_Record ((Instance11, Instance12));
      Source1 := New_Vertex;
      Target1 := New_Vertex;
      Connect (Source1, Target1);
      Elm1 := New_Vertex;
      Elm12 := New_Vertex;
      Elm2 := New_Vertex;
      Set1 := New_Set ((Elm1, Elm12));
      Set2 := New_Set ((Elm2, Elm12));
   end;

   procedure Open_Graph is
   begin
     Open ("test");
   end;

   procedure Create_Graph is
   begin
     Create ("test");
   end;

   procedure Test_Calculus is
      Fail : exception;
   begin
      if Extract (Targets (Singleton (Source1))) /= Target1
      or Extract (Sources (Singleton (Target1))) /= Source1
      then
         raise Fail;
      end if;
   end;

   ---------------------------
   -- CHUNK ITEM PROCESSORS --
   ---------------------------

   procedure Connection_Test (I, J : Positive) is
      Src1 : Vertex := To_Vertex ("s1" & I'Img);
      Src2 : Vertex := To_Vertex ("s2" & J'Img);
      Tgt : Vertex := To_Vertex ("tg" & I'Img & J 'Img);
   begin
      Connect (Src1, Tgt);
      Connect (Src2, Tgt);
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

   procedure Extract_Singleton_Test (I, J : Positive) is
      Src1 : Vertex := To_Vertex ("s1" & I'Img);
   begin
      Assert (Extract (Singleton (Src1)) = Src1, "extract singleton");
   end;


begin

   Perform (Create_Graph'Unrestricted_Access, "create", Fatal => True);
   Perform (Populate'Unrestricted_Access, "populate");
   Assert
     (Connected (Top, Branch1) and Connected (Top, Branch2), "connected");
   Assert
     (Get_Element (Top, 1) = Branch1 or
      Get_Element (Top, 2) = Branch1,
      "get_element");
   Assert
     (Get_Instance (Rec1, To_Vertex ("at11")) = Instance11,
      "get_instance");
   Assert
     (Get_Value (Rec1, To_Vertex ("at11")) = To_Vertex ("av11"),
      "get_value");
   Assert
     (Extract (Singleton (Top)) = Top, "extract singleton");
   Assert
     (Is_In (Top, Singleton (Top)), "is_in");
   Assert
     (Extract (Intersect (Singleton (Elm1), Singleton (Elm1))) = Elm1,
      "extract intersect");
   Assert
     (Extract (Intersect
        (Targets (Singleton (Set1)),
         Targets (Singleton (Set2)))) = Elm12,
      "extract intersect targets");
   Perform (Test_Calculus'Unrestricted_Access, "test_calculus");

   Assert
     (Extract (To_Selection (Set1)) = Elm1
      or Extract (To_Selection (Set1)) = Elm12,
      "extract to_selection");

   Assert
     (Extract (To_Selection (Set1) - Singleton (Elm1)) = Elm12,
      "extract subtract");

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
      Time_Increase_Test (M, N, Max, Max_Avg, Common_Targets_Test'Unrestricted_Access, "common targets");
      Time_Increase_Test (M, N, Max, Max_Avg, Extract_Singleton_Test'Unrestricted_Access, "extract singleton");
   end;

   Perform (Close'Unrestricted_Access, "close");
   Report;
end;
