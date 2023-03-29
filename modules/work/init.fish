# Set this variable so that every go dependency under the company's repository is flagged as
# private, thus skipping proxy and checksum database -- and consecutively avoiding their public
# exposure.

set --global --export GOPRIVATE github.com/mercadolibre
