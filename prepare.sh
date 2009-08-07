uuagc -dcfsprw --module=Text.Xml.Xml            -P src src/Text/Xml/Xml.ag &&
uuagc -dcfsprw --module=Text.Json.Json          -P src src/Text/Json/Json.ag &&
uuagc -dcfsprw --module=Text.Document.Core.Type -P src src/Text/Document/Core/Type.ag &&

# Prevent CPP warnings.
echo >> src/Text/Xml/Xml.hs &&
echo >> src/Text/Json/Json.hs &&
echo >> src/Text/Document/Core/Type.hs

