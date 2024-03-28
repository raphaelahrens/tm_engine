# A Dataflow process
# The member varibales are just examples

enum Classification {
    TOP_SECRET,
    SECRET,
    RESTRICTED,
    PUBLIC,
}

type Controls {
    isHardened = false,
    sanitizesInput = false,
    encodesOutput = false,
    authorizesSource = false,
    hasAccessControl = false,
}

type Data {
    classification=Classification.PUBLIC,
}

type Flow {
        protocol:String,
        dstPort:Int,
        data: Data,
}
type Actor {
    inBoundary: Boundary|String,
}


type Server {
    OS: String,
    controls: Controls
}

enum DatastoreType {
    SQL,
}

type Datastore {
    OS: String,
    controls: Controls,
    inBoundary:Boundary,
    type:DatastoreType,
    inScope = true,
    maxClassification:Classification,
    storesPII = false,
}

type Lambda{
    controls: Controls,
    inBoundary:Boundary,
}

