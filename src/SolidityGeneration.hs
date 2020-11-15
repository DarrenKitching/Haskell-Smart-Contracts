module SolidityGeneration where
import Contracts

data SolidityString = SolidityString String

class (SolidityVariable v) where
  createVariable :: v -> String
  createGetter :: v -> String
  createSetter :: v -> String

instance SolidityVariable (SolidityString) where
  createVariable (SolidityString s) = "string " ++ s ++ ";"  -- e.g. string value;
  createGetter (SolidityString s) = "function get() public view returns(string) {\n\treturn " ++ s ++ ";\n}"
  createSetter (SolidityString s) = "function set(string _" ++ s ++ ") public {\n\t" ++ s ++ " = _" ++ s ++ ";\n}" 