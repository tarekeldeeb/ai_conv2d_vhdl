----------------------------------------------------------------------------------
-- Company: 
-- Author: 
-- 
-- Create Date:    10:16:14 08/22/2019 
-- Design Name: 
-- Module Name:    memory_tb_mux - Behavioral 
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

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;

entity memory_tb_mux is
  	Generic (
		width		: integer := 16); 
	port(
		OC_Address	: in  STD_LOGIC_VECTOR(awidth-1 downto 0);
		OC_Valid  	: in  STD_LOGIC;
		OC_Data   	: in  STD_LOGIC_VECTOR(width-1 downto 0);
		tb_Address	: in  STD_LOGIC_VECTOR(awidth-1 downto 0);
		tb_Valid  	: in  STD_LOGIC;
		tb_Data   	: in  STD_LOGIC_VECTOR(width-1 downto 0);
		mem_Address : out STD_LOGIC_VECTOR(awidth-1 downto 0);
		mem_Valid  	: out STD_LOGIC;
		mem_Data   	: out STD_LOGIC_VECTOR(width-1 downto 0));
end memory_tb_mux;

architecture Behavioral of memory_tb_mux is

begin

  mem_Valid  	<= tb_Valid OR OC_Valid; 
  mem_Address	<= tb_Address when tb_Valid='1' else OC_Address;
  mem_Data   	<= tb_Data when tb_Valid='1' else OC_Data; 

end Behavioral;

