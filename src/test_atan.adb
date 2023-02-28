with Ada.Text_IO; use Ada.Text_IO;
with Celestial; use Celestial;
procedure Test_Atan is

   Root_3 : constant Celestial_Real := Sqrt (3.0);
   Ra, Rb : Degrees;
   X, Y : Celestial_Real;

begin -- Test_Atan
   X := 1.0;
   Y := Root_3;
   Ra := Arctan (Y, X);
   Rb := Arctan (Y / X);
   Put_Line ("(" & X'Img & "," & Y'Img & ") Arctan (Y, X):" & Ra'Img &
               " Book table 4.1:" & Rb'Img);
   X := -1.0;
   Y := Root_3;
   Ra := Arctan (Y, X);
   Rb := Arctan (Y / X) + 180.0;
   Put_Line ("(" & X'Img & "," & Y'Img & ") Arctan (Y, X):" & Ra'Img &
               " Book table 4.1:" & Rb'Img);
   X := 1.0;
   Y := -Root_3;
   Ra := Arctan (Y, X) + 360.0;
   Rb := Arctan (Y / X) + 360.0;
   Put_Line ("(" & X'Img & "," & Y'Img & ") Arctan (Y, X) + 360.0:" & Ra'Img &
               " Book table 4.1:" & Rb'Img);
   X := -1.0;
   Y := -Root_3;
   Ra := Arctan (Y, X) + 360.0;
   Rb := Arctan (Y / X) + 180.0;
   Put_Line ("(" & X'Img & "," & Y'Img & ") Arctan (Y, X) + 360.0:" & Ra'Img &
               " Book table 4.1:" & Rb'Img);
end Test_Atan;
