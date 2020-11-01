-- PROCEDURE MNESON.TOOLS.MNTEXT (BODY ONLY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Mneson.Tools.Mntext_Compiler;

procedure Mneson.Tools.Mntext is

   use Mneson.Tools.Mntext_Compiler;
   use Ada.Command_Line;
   use Ada.Text_IO;
   Name : String := Argument (1);
   C : Character;

begin

   Create (Name);
   Reset_Compiler;
   while not End_Of_File loop
      Get_Immediate (C);
      Feed_Compiler (C);
   end loop;
   Put (Img (Compile));
   Close;

end;
   