#!/bin/sh
#echo "# Customer list in XML:"
#echo ""
#curl -s 'http://localhost:3000/v1.0/customer' | xmllint --format -
#echo ""
#echo "-------------"
echo "# Customer list in JSON:"
echo ""
echo "curl -s 'http://localhost:3000/v1.0/customer?type=json' | python -mjson.tool"
curl -s 'http://localhost:3000/v1.0/customer?type=json' | python -mjson.tool
echo ""
echo "-------------"
#echo "# Invoice list in XML:"
echo "# Invoice list in JSON:"
echo ""
#curl -s 'http://localhost:3000/v1.0/invoice' | xmllint --format -
echo "curl -s 'http://localhost:3000/v1.0/invoice?type=json' | python -mjson.tool"
curl -s 'http://localhost:3000/v1.0/invoice?type=json' | python -mjson.tool
echo ""
echo ""
echo "-------------"
echo "# Creating invoice:"
echo ""
echo "curl -s -X POST 'http://localhost:3000/v1.0/invoice' -H \"Content-Type:application/json\" -d '{\"customer\":{\"name\":\"adam\",\"password\":\"1234\"},\"invoice\":{\"title\":\"Interesting Blog Invoice\",\"content\":\"I will finish this later\"}}' | python -mjson.tool"
curl -s -X POST 'http://localhost:3000/v1.0/invoice' -H "Content-Type:application/json" -d '{"customer":{"name":"adam","password":"1234"},"invoice":{"title":"Interesting Blog Invoice","content":"I will finish this later"}}' | python -mjson.tool
echo "-------------"
echo "# Creating invoice with the same title:"
echo ""
echo "curl -s -X POST 'http://localhost:3000/v1.0/invoice' -H \"Content-Type:application/json\" -d '{\"customer\":{\"name\":\"adam\",\"password\":\"1234\"},\"invoice\":{\"title\":\"Interesting Blog Invoice\",\"content\":\"I will finish this later\"}}' | python -mjson.tool"
curl -s -X POST 'http://localhost:3000/v1.0/invoice' -H "Content-Type:application/json" -d '{"customer":{"name":"adam","password":"1234"},"invoice":{"title":"Interesting Blog Invoice","content":"I will finish this later"}}' | python -mjson.tool
echo "-------------"
echo "# Commenting on Invoice 0"
echo ""
echo "curl -s -X POST 'http://localhost:3000/v1.0/invoice/id/0/comment' -H \"Content-Type:application/json\" -d '{\"customer\":{\"name\":\"adam\",\"password\":\"1234\"},\"comment\":\"I really outdid myself this time!?\"}' | python -mjson.tool"
curl -s -X POST 'http://localhost:3000/v1.0/invoice/id/0/comment' -H "Content-Type:application/json" -d '{"customer":{"name":"adam","password":"1234"},"comment":"I really outdid myself this time!?"}' | python -mjson.tool
echo "-------------"
echo "# Listing a invoices comments (Invoice 0)"
echo ""
echo "curl 'http://localhost:3000/v1.0/invoice/id/0/comment' -H \"Content-Type:application/json\" | python -mjson.tool"
curl 'http://localhost:3000/v1.0/invoice/id/0/comment' -H "Content-Type:application/json" | python -mjson.tool
