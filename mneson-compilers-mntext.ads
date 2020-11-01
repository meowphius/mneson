-- PACKAGE MNESON.COMPILERS.MNTEXT (SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Strings.Unbounded;
with AI302.Containers.Vectors;
with Mneson.Templates;
with Mneson.Types;

generic

   with package Work is new Mneson.Templates.Work (<>);

package Mneson.Compilers.Mntext is

   ---------------
   -- TOKENIZER --
   ---------------

   use Ada.Strings.Unbounded;
   use Mneson.Types;

   type Token_Classes is
     (White_Space, Text,
      Open_Block, Close_Block,
      Open_Set, Close_Set, Separator,
      Escape);

   type Token_Type is
      record
         Class : Token_Classes;
         Form : Unbounded_String;
         Delimited : Boolean;
         Line : Positive_64;
         Column : Natural_64;
      end record;

   type Process_Token is access procedure (Token : Token_Type);

   procedure Setup_Tokenizer (Process : Process_Token);
   procedure Advance_Tokenizer (C : Character);
   procedure Finish_Tokenizer;

   --------------
   -- COMPILER --
   --------------

   procedure Reset_Compiler;
   procedure Feed_Compiler (C : Character);
   function Compile return Vertex;

private

   type Item_Classes is (Text, Block, Set);

   type Item_Type is
      record
         Class : Item_Classes;
         First : Positive;
         Last : Positive;
      end record;

   use AI302.Containers;
   package Token_Vectors is new Vectors (Positive, Token_Type);
   package Item_Vectors is new Vectors (Positive, Item_Type);

end;
