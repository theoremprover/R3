{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE QuasiQuotes,LambdaCase #-}

module MachineSpec where

import Language.C.Quote.GCC
import Text.PrettyPrint.Mainland (prettyCompact)
import Text.PrettyPrint.Mainland.Class (ppr)
import System.FilePath

import CommandLine


data Endianness = Little | Big deriving (Read,Show)
data MachineSpec = MachineSpec {
	intSize::Int, longSize::Int, longLongSize::Int, endianness::Endianness
	} deriving (Read,Show)

machinespec_name = "machinespec" :: String

getMachineSpec :: FilePath -> IO MachineSpec
getMachineSpec compiler = do
	let
		srcfilename = machinespec_name <.> "c"
		exefilename = machinespec_name <.> "exe"
	compileHereIO compiler ["-o",exefilename,srcfilename] srcfilename $
		prettyCompact $ ppr machinespec_src
	(machinespec_s,"") <- runHereIO "." exefilename []
	return $ read machinespec_s

	where
	incl_stdio = "#include <stdio.h>"
	machinespec_src = [cunit|
$esc:incl_stdio
int main(void)
{
    char* little = "Little";
    char* big = "Big";
    char* endianness;
    unsigned char arr[2];
    *((unsigned short *)arr) = 255;
    // big endian means MSB is stored at smallest address.
    if(arr[0]==255 && arr[1]==0) endianness=little; else { if(arr[0]==0 && arr[1]==255) endianness=big; else
    	{ printf ("ERROR: Could not determine Endianness!\n"); return(1); } }
    printf("MachineSpec { intSize=%i, longSize=%i, longLongSize=%i, endianness=%s }\n",
        sizeof(int)*8,sizeof(long int)*8,sizeof(long long int)*8,endianness);
    return 0;
}
|]
