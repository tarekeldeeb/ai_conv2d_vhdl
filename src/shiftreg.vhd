----------------------------------------------------------------------------------
-- Author: Tarek Eldeeb
-- 
-- Create Date:    19:45:37 07/15/2019 
-- Module Name:    shiftreg - Behavioral 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity shiftreg is
    Generic (slength: integer:= 6);
    Port ( clk : in  STD_LOGIC;
           din : in  STD_LOGIC;
           dout : out  STD_LOGIC_VECTOR (slength-1 downto 0));
end shiftreg;

architecture Behavioral of shiftreg is
  Signal d_out: STD_LOGIC_VECTOR (slength-1 downto 0):=(others=>'0');
begin
Shift: process(clk)
begin
	if rising_edge(clk) then
	  d_out	<= d_out(slength-2 downto 0) & din;
	end if;
end process;
dout <= d_out;
end Behavioral;
