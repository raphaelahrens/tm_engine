enum Boundary {
    # Internet
    Internet,
    # Server/DB
    ServerDB,
    # AWS VPS
    VPC,
}

import dfd
import web
import db

# User
user = Actor{
    inBoundary = Boundary.Internet,
}

# Web Server
web = Server{
    OS = "Ubuntu",
    controls = Controls{
        isHardened = true,
        sanitizesInput = false,
        encodesOutput = true,
        authorizesSource = false,
    }
}
#sourceFiles = ["pytm/json.py", "docs/template.md"]

# SQL Database
db = Datastore {
    OS = "CentOS",
    controls = Controls{
        isHardened = true,
    },
    inBoundary = Boundary.ServerDB,
    type = DatastoreType.SQL,
    inScope = true,
    maxClassification = Classification.RESTRICTED,
}
# Real Identity Database
secretDb = Datastore{
    OS = "CentOS",
    controls = Controls{
        isHardened = true,
    },
    inBoundary = Boundary.ServerDB,
    type = DatastoreType.SQL,
    inScope = true,
    storesPII = true,
    maxClassification = Classification.TOP_SECRET,
}
#sourceFiles = ["pytm/pytm.py"],

# AWS Lambda
my_lambda = Lambda{
    controls= Controls{
        hasAccessControl = true,
    },
    inBoundary = Boundary.VPC,
}

# Database verify real user identity
# note : Verifying that the user is who they say they are.
db -> secretDb = Flow{ 
    protocol = "RDA-TCP",
    dstPort = 40234,
    # Token verifying user identity
    data = Data{
        classification = Classification.SECRET,
    },
}

# Comment sequence
#
# This sequence describes how a comment is added by the user.
seq insert_comment {
    # User enters comments
    # note: This is a simple web app
    # that stores and retrieves user comments.
    user -> web = HttpGet{
        # Comments in HTML or Markdown
        data = Data{},
    }

    #  Insert query with comments)
    #
    #   Web server inserts user comments\ninto it's SQL query and stores them in the DB.
    web -> db = Flow{
        protocol = "MySQL",
        dstPort = 3306,
        # Insert query with comments, 
        data = Data{},
    }

    #  Retrieve comments
    db -> web = Flow {
        protocol = "MySQL",
        dstPort = 80,
        #Web server retrieves comments from DB 
        data = Data{},
    }
    # Show comments
    web -> user = HttpReply {
        # Web server shows comments to the end user
        data = Data{},
    }
}

# Serverless function periodically cleans DB
my_lambda -> db = Flow {
    protocol = "MySQL",
    dstPort = 3306,
    #Serverless function clears DB
    data = Data{},
}

type Some {
    a: [Int]
}

a = Some {
    a = [1]
}
