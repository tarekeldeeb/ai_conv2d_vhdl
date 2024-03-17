----------------------------------------------------------------------------------
-- Company: 
-- Author: 
-- 
-- Create Date:    12:15:41 10/30/2019 
-- Design Name: 
-- Module Name:    MAC - Behavioral 
-- Project Name: 
-- Target Devices: 
-- License:  Check "LICENSE" which comes with this distribution for more informationg. 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;

entity MAC is
  Generic(
    width    : integer := 8;
	 isSigned : boolean := true);
  Port(
    A,B,C    : in  STD_LOGIC_VECTOR (width-1 downto 0);
    C_out    : out STD_LOGIC_VECTOR (width-1 downto 0));

end MAC;

architecture Behavioral of MAC is
  signal prod  : STD_LOGIC_VECTOR (2*width-1 downto 0);

begin
	-- TODO: Handle overflows!

MAC_SIGNED: if isSigned = true generate
	prod			<= std_logic_vector(signed(A)*signed(B_sel));
	C_out			<= std_logic_vector((signed(C)) + signed(prod(width-1 downto 0)));
end generate;

MAC_UNSIGNED: if isSigned = false generate
	prod			<= std_logic_vector(unsigned(A)*unsigned(B_sel));
	C_out			<= std_logic_vector((unsigned(C)) + unsigned(prod(width-1 downto 0)));
end generate;

end Behavioral;

