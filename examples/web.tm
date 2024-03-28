type HttpGet{
    protocol = "HTTP",
    dstPort = 80,
    data: Data
}
type HttpReply{
    protocol = "HTTP",
    srcPort = 80,
    data: Data
}
