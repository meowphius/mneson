-- PACKAGE MNESON.COMPILERS.MNTEXT (GENERIC BODY)
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
with AI302.Containers.Vectors;
with Mneson.Monitors;

package body Mneson.Compilers.Mntext is

   use Work;

   ------------------------
   -- INC/DEC PROCEDURES --
   ------------------------

   procedure Inc (X : in out Integer_64) is begin X := X + 1; end;
   procedure Dec (X : in out Integer_64) is begin X := X - 1; end;
   procedure Inc (X : in out Integer) is begin X := X + 1; end;
   procedure Dec (X : in out Integer) is begin X := X - 1; end;

   ---------------
   -- TOKENIZER --
   ---------------

   use Ada.Characters.Handling;
   use Ada.Characters.Latin_1;
   use Ada.Strings;

   Special_Chars : constant String (1 .. 5) := "(){},";
   Special_Class : constant array (1 .. 5) of Token_Classes :=
      (Open_Block, Close_Block, Open_Set, Close_Set, Separator);
   Escape_Char : constant Character := Reverse_Solidus;
   Trim_Char : constant Character := Vertical_Line;

   State : Token_Type;
   Settable_Process : Process_Token;
   type Open_Close_Classes is
      (Open_Set, Close_Set, Open_Block, Close_Block, Nope);
   Open_Close_Count : array (Open_Close_Classes) of Natural;

   procedure Monitor (S : String) renames Mneson.Monitors.Null_Monitor;

   procedure Restart_State is
   begin
      State.Class := White_Space;
      State.Form := Null_Unbounded_String;
      State.Delimited := False;
   end;

   procedure Restart_State_Including_Position is
   begin
      Restart_State;
      State.Line := 1;
      State.Column := 0;
   end;

   procedure Setup_Tokenizer (Process : Process_Token) is
   begin
      Settable_Process := Process;
      Restart_State_Including_Position;
      Open_Close_Count := (others => 0);
   end;

   function Open_Close_Class
     (Class : Token_Classes) return Open_Close_Classes is
   begin
     return Open_Close_Classes'Value
       (Token_Classes'Image (Class));
   exception
     when Constraint_Error => return Nope;
   end;

   procedure Output (Token : Token_Type) is
   begin
      Settable_Process (Token);
      Inc (Open_Close_Count (Open_Close_Class (Token.Class)));
   end;

   function Is_White_Space (C : Character) return Boolean is
   begin
      return C = ' ' or Is_Control (C);
   end;

   function US (C : Character) return Unbounded_String is
   begin
      return To_Unbounded_String (String' (1 => C));
   end;

   procedure Advance_Tokenizer (C : Character) is
      I : Integer;
   begin
      if C = LF then
         Inc (State.Line);
         State.Column := 0;
      else
         Inc (State.Column);
      end if;
      if State.Class = White_Space then
         if Is_White_Space (C) then
            Append (State.Form, C);
         elsif C = '"' then
            Output (State);
            State.Class := Text;
            State.Form := Null_Unbounded_String;
            State.Delimited := True;
         else
            I := Ada.Strings.Fixed.Index
              (Special_Chars, String' (1 => C));
            if I > 0 then
               Output (State);
               Output
                 ((Class => Special_Class (I),
                   Form => US (C),
                   Delimited => False,
                   Line => State.Line,
                   Column => State.Column));
               Restart_State;
            else
               Output (State);
               State.Class := Text;
               State.Form := US (C);
               State.Delimited := False;
            end if;
         end if;
      elsif State.Class = Text then
         if C = Escape_Char then
            State.Class := Escape;
         elsif State.Delimited then
            if C = Trim_Char then
               Trim (State.Form, Right);
            elsif C = '"' then
               Output (State);
               Restart_State;
            else
               Append (State.Form, C);
            end if;
         else
            if Is_White_Space (C) then
               Output (State);
               State.Class := White_Space;
               State.Form := US (C);
               State.Delimited := False;            
            else
               I := Ada.Strings.Fixed.Index
                 (Special_Chars, String' (1 => C));
               if I > 0 then
                 Output (State);
                 Output
                   ((Class => Special_Class (I),
                     Form => US (C),
                     Delimited => False,
                     Line => State.Line,
                     Column => State.Column));
                 Restart_State;
               else
                 Append (State.Form, C);
               end if;
            end if;
         end if;
      elsif State.Class = Escape then
         Append (State.Form, C);
         State.Class := Text;
      end if;
   end;

   function Check_Open_Close_Count return Boolean is
      I : Open_Close_Classes;
   begin
      I := Open_Close_Classes'First;
      while I /= Nope loop
         if Open_Close_Count (I)
            /= Open_Close_Count (Open_Close_Classes'Succ (I)) then
               return False;
         end if;
         I := Open_Close_Classes'Succ (Open_Close_Classes'Succ (I));
      end loop;
      return True;
   end;

   procedure Finish_Tokenizer is
   begin
      Output (State);
      if not Check_Open_Close_Count then raise Syntax_Error; end if;
   end;

   --------------
   -- COMPILER --
   --------------

   Tokens : Token_Vectors.Vector_Type;

   type Item_Array is array (Positive range <>) of Item_Type;

   function To_Array (V : Item_Vectors.Vector_Type) return Item_Array;
   function Itemize (First, Last : Positive) return Item_Array;
   function Compile (Item : Item_Array) return Vertex;
   function Compile (Item : Item_Type) return Vertex;

   function Position_Image (Token : Token_Type) return String is
   begin
      return 
         Positive_64'Image (Token.Line) &
         " :" & Natural_64'Image (Token.Column);
   end;

   function To_Array (V : Item_Vectors.Vector_Type) return Item_Array is
      use Item_Vectors;
      A : Item_Array (1 .. Natural (Length (V)));
   begin
      for I in A'Range loop
         A (I) := Element (V, I);
      end loop;
      return A;
   end;

   function Compile (Token : Token_Type) return Vertex is
   begin
      Monitor ("compiling token [" & To_String (Token.Form) & "], at"&
         Position_Image (Token));
      return Val (To_String (Token.Form));
   end;            

   function Compile (Item : Item_Type) return Vertex is
      use Token_Vectors;
      Y : Vertex;
      I, K : Positive;
      N : Natural;
   begin
      Monitor ("compiling " & Item_Classes'Image (Item.Class) &
         "item");
      if Item.Class = Text then
         return Compile (Element (Tokens, Item.First));
      elsif Item.Class = Block then
         return Compile (Itemize (Item.First, Item.Last));
      elsif Item.Class = Set then
         Y := New_Vertex;
         if Item.Last >= Item.First then
           K := Item.First;
           I := K;
           N := 0;
           loop
             if I = Item.Last then
               Connect (Y, Compile (Itemize (K, I)));
               exit;
             elsif Element (Tokens, I).Class = Open_Set then Inc (N);
             elsif Element (Tokens, I).Class = Close_Set then Dec (N);
             elsif Element (Tokens, I).Class = Separator and N = 0 then
               Connect (Y, Compile (Itemize (K, I - 1)));
               K := I + 1;
             end if;
             Inc (I);
           end loop;
         end if;
         return Y;
      else
         raise Program_Error;
      end if;
   end;

   function Debug_Info (Item : Item_Type) return String is
      use Token_Vectors;
      N : Natural := Item.Last - Item.First + 1;
      U : Unbounded_String;
   begin
      Append (U,
         Item_Classes'Image (Item.Class) & " item," &
         Natural'Image (N) & " tokens");
      if N > 0 then
         Append (U,
           ", positions" & Position_Image (Element (Tokens, Item.First)) &
           " to" & Position_Image (Element (Tokens, Item.Last)));
      end if;
      return To_String (U);
   exception
      when others => return "error getting debug info";
   end;

   function Compile (Item : Item_Array) return Vertex is
      use Ada.Exceptions;
      Y : Vertex;
   begin
      if Item'Length = 1 then
         return Compile (Item (Item'First));
      elsif Item'Length > 1 then
         Y := New_Vertex;
         Connect (Compile (Item (Item'First)), Y);
         Connect (Y, Compile (Item (Item'First + 1 .. Item'Last)));
         return Y;
      else
         raise Program_Error;
      end if;
   exception
      when E : others =>
         if Item'Length >= 1 then
            Raise_Exception
              (Exception_Identity (E),
               Debug_Info (Item (Item'First)));
         else
            Raise_Exception
              (Exception_Identity (E),
               "(null item array)");
         end if;
   end;

   function Itemize (First, Last : Positive) return Item_Array is
      V : Item_Vectors.Vector_Type;
      I, K : Positive;
      N : Natural;
      use Item_Vectors;
      use Token_Vectors;
   begin
      Monitor ("itemizing" & First'img & last'img);
      I := First;
      while I <= Last loop
         if Element (Tokens, I).Class = Text then
            Append (V, (Text, I, I));
         elsif Element (Tokens, I).Class = Open_Block then
            K := I + 1;
            N := 1;
            while N > 0 loop
               Inc (I);
               if Element (Tokens, I).Class = Open_Block then Inc (N);
               elsif Element (Tokens, I).Class = Close_Block then Dec (N);
               end if;
            end loop;
            Append (V, (Block, K, I - 1));
         elsif Element (Tokens, I).Class = Open_Set then
            K := I + 1;
            N := 1;
            while N > 0 loop
               Inc (I);
               if Element (Tokens, I).Class = Open_Set then Inc (N);
               elsif Element (Tokens, I).Class = Close_Set then Dec (N);
               end if;
            end loop;
            Append (V, (Set, K, I - 1));
         elsif Element (Tokens, I).Class = White_Space then
            null;
         else
            raise Program_Error;
         end if;
         Inc (I);
      end loop;
      return To_Array (V);
   end;

   procedure Append_Token (Token : Token_Type) is
      use Token_Vectors;
   begin
      if Token.Class /= White_Space then
         Append (Tokens, Token);
      end if;
   end;

   procedure Reset_Compiler is
      use Token_Vectors;
   begin
      Clear (Tokens);
      Setup_Tokenizer (Append_Token'Unrestricted_Access);
   end;

   procedure Feed_Compiler (C : Character) is
   begin
      Advance_Tokenizer (C);
   end;

   function Compile return Vertex is
      use Token_Vectors;
   begin
      Finish_Tokenizer;
      declare
        Items : Item_Array := Itemize (1, Positive (Length (Tokens)));
      begin
        return Compile (Items);
      end; 
  end;

end;
