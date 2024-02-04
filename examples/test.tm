import dfd

enum Boundary {
    # Internet
    Internet,
    # Server/DB
    ServerDB,
    # AWS VPS
    VPC,
}

enum Classification {
    TOP_SECRET,
    SECRET,
    RESTRICTED,
    PUBLIC,
}

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
    }
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
    }
    inBoundary = server_db,
    type = DatastoreType.SQL,
    inScope = true,
    storesPII = true,
    maxClassification = Classification.TOP_SECRET,
}
#sourceFiles = ["pytm/pytm.py"],

# AWS Lambda
my_lambda = Lambda{
    controls.hasAccessControl = true,
    inBoundary = vpc,
}

# Database verify real user identity
# note : Verifying that the user is who they say they are.
db -> secretDb = Flow{ 
                     protocol = "RDA-TCP",
                     dstPort = 40234,
                     # Token verifying user identity
                     data = Data{
                         classification=Classification.SECRET,
                     },
                     maxClassification = Classification.SECRET,
                 }

# Comment sequence
#
# This sequence describes how a comment is added by the user.
seq insert_comment {
    # User enters comments
    # note: This is a simple web app
    # that stores and retrieves user comments.
    user -> web = Flow{
        protocol = "HTTP",
        dstPort = 80,
        # Comments in HTML or Markdown
        data = Data{
            classification=Classification.PUBLIC
        },
    }

    #  Insert query with comments)
    #
    #   Web server inserts user comments\ninto it's SQL query and stores them in the DB.
    web -> db = Flow{
        protocol = "MySQL",
        dstPort = 3306,
        # Insert query with comments, 
        data = Data{
            classification=Classification.PUBLIC
        },
    )

    comment_retrieved = Data(
        "Web server retrieves comments from DB", classification=Classification.PUBLIC
    )
    #  Retrieve comments
    db -> web = Flow {
        protocol = "MySQL",
        dstPort = 80,
        #Web server retrieves comments from DB 
        data = Data{
            classification=Classification.PUBLIC
        }
        responseTo = web_to_db

    # Show comments
    web -> user = Flow {
        protocol = "HTTP".
        # Web server shows comments to the end user
        data = Data{
            classifcation=Classification.PUBLIC
        }
        responseTo = user_to_web
    }
}

# Serverless function periodically cleans DB
my_lambda -> db = Flow {
    protocol = "MySQL"
    dstPort = 3306
    #Serverless function clears DB
    data = Data{
        classification=Classification.PUBLIC,
    }
}

# User Id Token
#
# Some unique token that represents the user real data in the secret database
userIdToken = Data(
    name="User ID Token",
    classification=Classification.TOP_SECRET,
)
# traverses=[user_to_web, db_to_secretDb],
# processedBy=[db, secretDb],
