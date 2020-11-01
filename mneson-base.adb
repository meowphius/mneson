-- PACKAGE MNESON.BASE (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Direct_IO;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.Bubble_Sort_A;
with Mneson.Monitors;

package body Mneson.Base is

   -- MORE TYPES
   subtype String_8 is String (1 .. 8);

   -- MORE PACKAGES
   package Short_String_IO is new Ada.Direct_IO (Character);

   -- TIP VALUES
   subtype Tiny_String_Tip  is  Natural_16 range 0 ..     8;
   subtype Short_String_Tip is  Natural_16 range 9 .. 50000;
   Long_String_Tip   : constant Natural_16 :=         50032; -- (*)
   Huge_String_Tip   : constant Natural_16 :=         50064; -- (*)
   Integer_Tip       : constant Natural_16 :=         51000;
   Float_Tip         : constant Natural_16 :=         52000; -- (*)
   Serial_Number_Tip : constant Natural_16 :=         53000;
   Front_Vertex_Tip  : constant Natural_16 :=         60001;
   Back_Vertex_Tip   : constant Natural_16 :=         60002;
   System_Tip        : constant Natural_16 :=         60003;
   -- (*) not implemented yet

   -- SPECIAL VERTICES
   Front_Vertex  : constant Vertex := (Front_Vertex_Tip, 0);
   Back_Vertex   : constant Vertex := (Back_Vertex_Tip, 0);
   Selections    : Vertex;

   -- GRAPH STATE
   use Ada.Strings.Unbounded;
   Current_Name       : Unbounded_String;
   Links              : Link_Sets.Set_Type;
   Inv_Links          : Link_Sets.Set_Type;
   String_Table       : String_Maps.Map_Type;
   Short_Strings_File : Short_String_IO.File_Type;
   Last_Serial_Number : Vertex_Count;
   Graph_Open         : Boolean := False;
   Monitor            : Process_String :=
                           Mneson.Monitors.Null_Monitor'Access;

   -- FILENAMES
   type Physical_Files is (Structures, Short_Strings);
   Suffix : constant String := ".mn";

   ------------------------
   -- INC/DEC PROCEDURES --
   ------------------------

   procedure Inc (X : in out Integer_64) is begin X := X + 1; end;
   procedure Dec (X : in out Integer_64) is begin X := X - 1; end;
   procedure Inc (X : in out Integer) is begin X := X + 1; end;
   procedure Dec (X : in out Integer) is begin X := X - 1; end;

   --------------------------------
   -- FILE MANAGEMENT OPERATIONS --
   --------------------------------

   function Filename
     (Name : String ; File : Physical_Files) return String is
   begin
      return Name & "-" & Physical_Files'Image (File) & Suffix;
   end;

   procedure Create_System is
   begin
      Selections := New_Vertex;
   end;

   procedure Delete_System is
   begin
      null; --Delete_All_Selections;
   end;

   procedure Create (Name: String) is
      Short_Strings_Filename : String := Filename (Name, Short_Strings);
      use Short_String_IO;
      use Ada.Exceptions;
      procedure Close_Any_Open_Files is
      begin
         Monitor ("Closing any open files");
         if Is_Open (Short_Strings_File) then
            Close (Short_Strings_File);
         end if;
      end;
   begin
      if Graph_Open then raise State_Error; end if;
      Monitor ("Creating file " & Short_Strings_Filename);
      begin
         Create (Short_Strings_File, Inout_File, Short_Strings_Filename);
      exception
         when E : others =>
            Monitor (Exception_Name (E));
            Close_Any_Open_Files;
            Monitor ("Create failed; graph not changed");
            raise;
      end;
      Monitor ("Initializing graph with zero elements");
      Link_Sets.Clear (Links);
      Link_Sets.Clear (Inv_Links);
      String_Maps.Clear (String_Table);
      Last_Serial_Number := 0;
      Current_Name := To_Unbounded_String (Name);
      Graph_Open := True;
      Create_System;
      Monitor ("Create complete");
   end;

   procedure Open (Name : String) is
      Structures_Filename : String := Filename (Name, Structures);
      Short_Strings_Filename : String := Filename (Name, Short_Strings);
      Structures_File : Ada.Streams.Stream_IO.File_Type;
      use Ada.Streams.Stream_IO;
      use Link_Sets;
      use String_Maps;
      use Short_String_IO;
      use Ada.Exceptions;
      procedure Close_Any_Open_Files is
      begin
         Monitor ("Closing any open files");
         if Is_Open (Structures_File) then
            Close (Structures_File);
         end if;
         if Is_Open (Short_Strings_File) then
            Close (Short_Strings_File);
         end if;
      end;
   begin
      if Graph_Open then raise State_Error; end if;
      begin
         Monitor ("Opening file " & Structures_Filename);
         Open (Structures_File, In_File, Structures_Filename);
       exception
         when E : others =>
            Monitor (Exception_Name (E));
            Close_Any_Open_Files;
            Monitor ("Open failed; graph not changed");
            raise;
      end;
      begin
         Monitor ("Opening file " & Short_Strings_Filename);
         Open (Short_Strings_File, Inout_File, Short_Strings_Filename);
      exception
         when E : others =>
            Monitor (Exception_Name (E));
            Close_Any_Open_Files;
            Monitor ("Open failed; graph not changed");
            raise;
      end;
      Monitor ("Reading Links");
      Link_Sets.Set_Type'Read (Stream (Structures_File), Links);
      Monitor ("Reading Inv_Links");
      Link_Sets.Set_Type'Read (Stream (Structures_File), Inv_Links);
      Monitor ("Reading String_Table");
      String_Maps.Map_Type'Read (Stream (Structures_File), String_Table);
      Monitor ("Graph initialization: final details");
      Vertex_Count'Read (Stream (Structures_File), Last_Serial_Number);
      Current_Name := To_Unbounded_String (Name);
      Monitor ("Closing file " & Structures_Filename);
      Close (Structures_File);
      Graph_Open := True;
      Create_System;
      Monitor ("Open complete");
   end;

   procedure Close is
      Name : String := To_String (Current_Name);
      Structures_Filename : String := Filename (Name, Structures);
      Structures_File : Ada.Streams.Stream_IO.File_Type;
      use Ada.Exceptions;
      use Ada.Streams.Stream_IO;
   begin
      if not Graph_Open then raise State_Error; end if;
      begin
         Monitor ("Opening file " & Structures_Filename & " for writing");
         Create (Structures_File, Out_File, Structures_Filename);
      exception
         when E : others =>
            Monitor (Exception_Name (E));
            if Is_Open (Structures_File) then
               Monitor ("Closing file " & Structures_Filename);
               Close (Structures_File);
            end if;
            Monitor ("Close failed; graph not changed");
            raise;
      end;
      Monitor ("Writing Links");
      Link_Sets.Set_Type'Write (Stream (Structures_File), Links);
      Monitor ("Writing Inv_Links");
      Link_Sets.Set_Type'Write (Stream (Structures_File), Inv_Links);
      Monitor ("Writing String_Table");
      String_Maps.Map_Type'Write (Stream (Structures_File), String_Table);
      Monitor ("Finalizing");
      Vertex_Count'Write (Stream (Structures_File), Last_Serial_Number);
      Close (Structures_File);
      Short_String_IO.Close (Short_Strings_File);
      Delete_System;
      Graph_Open := False;
      Monitor ("Close complete");
   end;

   procedure Save is
      Name : String := To_String (Current_Name);
   begin
      if not Graph_Open then raise State_Error; end if;
      Monitor ("Saving (Save is implemented as closing and reopening)");
      Close;
      Open (Name);
      Monitor ("Save complete");
   end;

   procedure Save_As (Name : String) is
      procedure Copy_File (From, To : String) is
         use Ada.Text_IO;
         From_File, To_File : File_Type;
         C : Character;
      begin
         Monitor ("Copying file " & From & " to " & To);
         Open (From_File, In_File, From);
         Create (To_File, Out_File, To);
         while not End_Of_File (From_File) Loop
            Get_Immediate (From_File, C);
            Put (To_File, C);
         end loop;
      exception
         when others =>
            if Is_Open (From_File) then Close (From_File); end if;
            if Is_Open (To_File) then Close (To_File); end if;
            raise;
      end;
      procedure Delete_File (Name : String) is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Monitor ("Deleting file " & Name);
         Open (File, Out_File, Name);
         Delete (File);
      end;
   begin
      if not Graph_Open then raise State_Error; end if;
      Monitor ("Saving as " & Name);
      Close;
      for I in Physical_Files loop
         Copy_File
           (From => Filename (To_String (Current_Name), I),
            To => Filename (Name, I));
      end loop;
      for I in Physical_Files loop
         null; --Delete_File (Filename (To_String (Current_Name), I));
      end loop;
      Open (Name);
      Monitor ("Save_As complete");
   end;

   procedure Set_Monitor (Monitor : Process_String) is
   begin
      Mneson.Base.Monitor := Monitor;
   end;

   procedure Set_Logging (On : Boolean) is
   begin
      raise Not_Implemented_Yet;
   end;

   function Logging_On return Boolean is
   begin
      raise Not_Implemented_Yet;
      return False;
   end;

   ---------------------------------
   -- VALUELESS VERTEX OPERATIONS --
   ---------------------------------

   function New_Serial_Number return Serial_Number is
   begin
      Inc (Last_Serial_Number);
      return Last_Serial_Number;
   end;

   function Valueless_Vertex (Number : Serial_Number) return Vertex is
   begin
      if Number > Last_Serial_Number then
         Last_Serial_Number := Number;
      end if;
      return (Serial_Number_Tip, Modular_64 (Number));
   end;

   function New_Vertex return Vertex is
   begin
      return Valueless_Vertex (New_Serial_Number);
   end;

   -------------------------------
   -- INTEGER VERTEX OPERATIONS --
   -------------------------------

   function To_Vertex (Value : Integer_64) return Vertex is
   begin
      return (Integer_Tip, Modular_64 (Value));
   end;

   function Value (X : Vertex) return Integer_64 is
   begin
      if X.Tip = Integer_Tip then
         return Integer_64 (X.Cue);
      else
         raise Type_Error;
      end if;
   end;

   ------------------------------
   -- STRING VERTEX OPERATIONS --
   ------------------------------

   function To_Modular_64 is new Ada.Unchecked_Conversion
     (Source => String_8, Target => Modular_64);

   function To_String_8 is new Ada.Unchecked_Conversion
     (Source => Modular_64, Target => String_8);

   function To_Vertex (Value : String) return Vertex is
      N : Natural_16 := Value'Length;
      X : Vertex;
      use Ada.Characters.Latin_1;
      use Ada.Strings.Fixed;
   begin
      if N <= Tiny_String_Tip'Last then
         X.Tip := Natural_16 (N);
         X.Cue := To_Modular_64 (String_8 (Head (Value, 8, NUL)));
      elsif N <= Short_String_Tip'Last then
         declare
            use String_Maps;
            use Short_String_IO;
            Found, Dont_Need : String_Maps.Cursor_Type;
            Expected_True : Boolean;
         begin
            Found := Find (String_Table, Value);
            if Found /= Null_Cursor then
               X := Element (Found);
            else -- new string
               X.Tip := Natural_16 (N);
               X.Cue := Modular_64 (Size (Short_Strings_File) + 1);
               Set_Index (Short_Strings_File, Positive_Count (X.Cue));
               for I in Value'Range loop
                  Write (Short_Strings_File, Value (I));
               end loop;
               Insert
                  (Map => String_Table,
                   Key => Value,
                   New_Item => X,
                   Cursor => Dont_Need,
                   Success => Expected_True);
            end if;
         end;
      else
         raise Not_Implemented_Yet;
      end if;
      return X;
   end;

   function Value (X : Vertex) return String is
   begin
      return Slice (X, 1, Length (X));
   end;

   function Length (X : Vertex) return Natural is
   begin
      if X.Tip in Tiny_String_Tip or X.Tip in Short_String_Tip then
         return Natural (X.Tip);
      elsif X.Tip = Long_String_Tip or X.Tip = Huge_String_Tip then
         raise Not_Implemented_Yet;
      else
         raise Type_Error;
      end if;
   end;

   function Slice
      (X : Vertex ; Low : Positive ; High : Natural) return String is
   begin
      if X.Tip in Tiny_String_Tip then
         return To_String_8 (X.Cue) (1 .. Natural (X.Tip)) (Low .. High);
      elsif X.Tip in Short_String_Tip then
         declare
            S : String (Low .. High);
            use Short_String_IO;
         begin
            Set_Index
              (Short_Strings_File,
               Positive_Count (X.Cue) + Positive_Count (Low) - 1);
            for I in S'Range loop
               Read (Short_Strings_File, S (I));
            end loop;
            return S;
         end;
      elsif X.Tip = Long_String_Tip then
         raise Not_Implemented_Yet;
      elsif X.Tip = Huge_String_Tip then
         raise Not_Implemented_Yet;
      else
         raise Type_Error;
      end if;
   end;

   ------------------
   -- VERTEX ORDER --
   ------------------

   function Is_Integer_64 (X : Vertex) return Boolean is
   begin
      return X.Tip = Integer_Tip;
   end;

   function Is_String (X : Vertex) return Boolean is
   begin
      return X.Tip in Tiny_String_Tip or X.Tip in Short_String_Tip;
   end;

   function Is_Valueless (X : Vertex) return Boolean is
   begin
      return X.Tip = Serial_Number_Tip;
   end;

   function LT_For_String_Vertices
     (Left, Right : Vertex) return Boolean is
   begin
      if Left.Tip in Tiny_String_Tip then
         if Right.Tip in Tiny_String_Tip then
            return String'(Value (Left)) < String'(Value (Right));
         elsif Right.Tip in Short_String_Tip then
            return True;
         else
            raise Not_Implemented_Yet;
         end if;
      elsif Left.Tip in Short_String_Tip then
         if Right.Tip in Tiny_String_Tip then
            return False;
         elsif Right.Tip in Short_String_Tip then
            return Left.Cue < Right.Cue;
         else
            raise Not_Implemented_Yet;
         end if;
      else
         raise Not_Implemented_Yet;
      end if;
   end;

   function "<" (Left, Right : Vertex) return Boolean is
   begin
      if Left = Right then
         return False;
      elsif Left = Front_Vertex or Right = Back_Vertex then
         return True;
      elsif Left = Back_Vertex or Right = Front_Vertex then
         return False;
      elsif Is_Integer_64 (Left) then
         if Is_Integer_64 (Right) then
            return
               Integer_64'(Value (Left)) < Integer_64'(Value (Right));
         else
            return True;
         end if;
      elsif Is_String (Left) then
         if Is_Integer_64 (Right) then
            return False;
         elsif Is_String (Right) then
            return LT_For_String_Vertices (Left, Right);
         elsif Is_Valueless (Right) then
            return True;
         else
            raise Program_Error;
         end if;
      elsif Is_Valueless (Left) then
         if Is_Valueless (Right) then
            return Left.Cue < Right.Cue;
         else
            return False;
         end if;
      else
         raise Program_Error;
      end if;
   end;

   -------------
   -- CONNECT --
   -------------

   procedure Connect (Source, Target : Vertex) is
      use Link_Sets;
      Dont_Need : Cursor_Type;
      Dont_Care : Boolean;
   begin
      Insert
        (Set => Links,
         New_Item => (Source, Target),
         Cursor => Dont_Need,
         Success => Dont_Care);
      Insert
        (Set => Inv_Links,
         New_Item => (Target, Source),
         Cursor => Dont_Need,
         Success => Dont_Care);
   end;

   ----------------
   -- DISCONNECT --
   ----------------

   procedure Disconnect (Source, Target : Vertex) is
      use Link_Sets;
   begin
      Delete (Links, (Source, Target));
      Delete (Inv_Links, (Target, Source));
   end;

   procedure Delete_All_In_Range
     (Link_Set : in out Link_Sets.Set_Type; From, To : Link_Type)
   is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Link_Set, From);
   begin
     while I /= Back (Link_Set) loop
       exit when Element (I) > To;
       Delete (Link_Set, I);
     end loop;
   end;

   procedure Disconnect_From_Targets (Source : Vertex) is
   begin
      Delete_All_In_Range
        (Link_Set => Links,
         From => (Source, Front_Vertex),
         To => (Source, Back_Vertex));
      Delete_All_In_Range
        (Link_Set => Inv_Links,
         From => (Front_Vertex, Source),
         To => (Back_Vertex, Source));
   end;

   procedure Disconnect_From_Sources (Target : Vertex) is
   begin
      Delete_All_In_Range
        (Link_Set => Links,
         From => (Front_Vertex, Target),
         To => (Back_Vertex, Target));
      Delete_All_In_Range
        (Link_Set => Inv_Links,
         From => (Target, Front_Vertex),
         To => (Target, Back_Vertex));
   end;

   procedure Disconnect (X : Vertex) is
   begin
      Disconnect_From_Targets (X);
      Disconnect_From_Sources (X);
   end;

   ----------------------------
   -- FOR EACH LINK IN RANGE --
   ----------------------------

   generic
      with procedure Process (Link : Link_Type) is <>;
   procedure For_Each_Link_In_Range
     (Set : Link_Sets.Set_Type; From, To : Link_Type);

   procedure For_Each_Link_In_Range
     (Set : Link_Sets.Set_Type; From, To : Link_Type)
   is
      use Link_Sets;
      I : Cursor_Type := Lower_Bound (Set, From);
      E : Link_Type;
   begin
      I := Lower_Bound (Set, From);
      while I /= Back (Set) loop
         E := Element (I);
         exit when E > To;
         Process (E);
         Increment (I);
      end loop;
   end;

   ----------------------------
   -- FOR EACH TARGET/SOURCE --
   ----------------------------

   generic
      Link_Set : Link_Sets.Set_Type;
      with procedure Process (Vertex_2 : Vertex) is <>;
   procedure For_Each_Vertex_2 (Vertex_1 : Vertex);

   procedure For_Each_Vertex_2 (Vertex_1 : Vertex) is
      procedure Process (Link : Link_Type) is
      begin
         Process (Link (2));
      end;
      procedure Process_Links is new For_Each_Link_In_Range;
   begin
      Process_Links
        (Set => Link_Set,
         From => (Vertex_1, Front_Vertex),
         To => (Vertex_1, Back_Vertex));
   end;

   generic
      with procedure Process (Target : Vertex) is <>;
   procedure Generic_For_Each_Target (Source : Vertex);

   procedure Generic_For_Each_Target (Source : Vertex) is
      procedure Process_Target is new For_Each_Vertex_2 (Links);
   begin
      Process_Target (Source);
   end;   

   generic
      with procedure Process (Source : Vertex) is <>;
   procedure Generic_For_Each_Source (Target : Vertex);

   procedure Generic_For_Each_Source (Target : Vertex) is
      procedure Process_Source is new For_Each_Vertex_2 (Inv_Links);
   begin
      Process_Source (Target);
   end;

   ---------------
   -- RECONNECT --
   ---------------

   procedure Reconnect (Source, Target, New_Target : Vertex) is
   begin
      Disconnect (Source, Target);
      Connect (Source, New_Target);
   end;

   procedure Inv_Reconnect (Target, Source, New_Source : Vertex) is
   begin
      Disconnect (Source, Target);
      Connect (New_Source, Target);
   end;

   ---------------
   -- CONNECTED --
   ---------------

   function Connected (Source, Target : Vertex) return Boolean is
   begin
      return Link_Sets.Is_In ((Source, Target), Links);
   end;

   function Connected (Source : Vertex) return Boolean is
      use Link_Sets;
      X : Cursor_Type := Lower_Bound (Links, (Source, Front_Vertex));
   begin
      return X /= Null_Cursor and then Element (X) (1) = Source;
   end;

   function Inv_Connected (Target : Vertex) return Boolean is
      use Link_Sets;
      X : Cursor_Type := Lower_Bound (Inv_Links, (Target, Front_Vertex));
   begin
      return X /= Null_Cursor and then Element (X) (1) = Target;
   end;

   ---------------------------
   -- GENERIC FOR EACH LINK --
   ---------------------------

   generic
      with procedure Process (Source, Target : Vertex) is <>;
   procedure Generic_For_Each_Link;

   procedure Generic_For_Each_Link is
      use Link_Sets;
      procedure Process (Link : Cursor_Type) is
         E : Link_Type := Element (Link);
      begin
         Process (E (1), E (2));
      end;
      procedure Traverse is new Generic_Iteration;
   begin
      Traverse (Links);
   end;

   ---------
   -- IMG --
   ---------

   function Img (X : Modular_64) return String is
      S : String := Modular_64'Image (X);
   begin
      return S (2 .. S'Last);
   end;

   function Img (X : Vertex) return String is
   begin
      if X = Front_Vertex then return "FRONT";
      elsif X = Back_Vertex then return "BACK";
      elsif Is_Integer_64 (X) then return Integer_64'Image (Value (X));
      elsif Is_String (X) then return '"' & Value (X) & '"';
      else return '#' & Img (X.Cue);
      end if;
   end;

   -- FUNCTION VAL MAY NEED IMPROVEMENT
   function Val (Img : String) return Vertex is
      X : Integer_64;
   begin
      begin
         X := Integer_64'Value (Img);
      exception
         when Constraint_Error => return To_Vertex (Img);
      end;
      return To_Vertex (X);
   end;
      
   function Escape (S : String) return String is
      use Ada.Strings.Fixed;
      Special_Chars : String := "\|""";
      U : Unbounded_String;
   begin
      for I in S'Range loop
         if Index (Special_Chars, S (I .. I)) > 0 then
            Append (U, '\');
         end if;
         Append (U, S (I));
      end loop;
      return To_String (U);
   end;

   --------------------------
   -- NON GENERIC FOR EACH --
   --------------------------

   procedure For_Each_Target (Source : Vertex; Process: Process_Vertex) is
      procedure Actual_For_Each_Target is
         new Generic_For_Each_Target (Process.all);
   begin
      Actual_For_Each_Target (Source);
   end;

   procedure For_Each_Source (Target : Vertex; Process: Process_Vertex) is
      procedure Actual_For_Each_Source is
         new Generic_For_Each_Source (Process.all);
   begin
      Actual_For_Each_Source (Target);
   end;

   procedure For_Each_Link (Process: Process_Link) is
      procedure Actual_For_Each_Link is
         new Generic_For_Each_Link (Process.all);
   begin
      Actual_For_Each_Link;
   end;

   procedure For_Each_Common_Target
     (Source_1, Source_2 : Vertex; Process : Process_Vertex)
   is
      use Link_Sets;
      Next1 : Cursor_Type := Lower_Bound (Links, (Source_1, Front_Vertex));
      Next2 : Cursor_Type := Lower_Bound (Links, (Source_2, Front_Vertex));
      Back1 : Cursor_Type := Lower_Bound (Links, (Source_1, Back_Vertex));
      Back2 : Cursor_Type := Lower_Bound (Links, (Source_2, Back_Vertex));
      Tgt1, Tgt2 : Vertex;
   begin
      while Next1 /= Back1 and Next2 /= Back2 loop
         Tgt1 := Element (Next1) (2);
         Tgt2 := Element (Next2) (2);
         if Tgt1 = Tgt2 then
            Process (Tgt1);
            Next1 := Succ (Next1);
            Next2 := Succ (Next2);
         elsif Tgt1 < Tgt2 then
            Next1 := Succ (Next1);
         elsif Tgt2 < Tgt1 then
            Next2 := Succ (Next2);
         end if;
      end loop;
   end;

end;

-- NOTES

-- The non-standard (GNAT-specific) Unrestricted_Access attribute
-- is used to simplify enourmously the callback idiom.
-- Ada 2005 will make this very simple, see AI-254.
