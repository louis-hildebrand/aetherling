exec 1>>compile_chisel.log 2>&1
echo ""
echo "--------------------------------------------------------------------------------"
echo ""

set -o xtrace
cp "$1" "${2}/src/main/scala/aetherling/modules/Top.scala"
cd "$2"
# If compilation fails, don't just copy over the previous Verilog code
rm -f "$2/Top.v"
sbt -mem 2048 "runMain aetherling.modules.Top"
cd -
cp "$2/Top.v" "$3"
