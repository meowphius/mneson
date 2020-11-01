-- PACKAGE MNESON.TOOLS (SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Ada.Command_Line;
with Ada.Text_IO;
with Mneson.Base;
with Mneson.Types;

package Mneson.Tools is

   package Graph is new Mneson.Base;
   use Graph;
   use Mneson.Types;
   procedure Open_Argument_1;

end;
