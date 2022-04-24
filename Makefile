##
# Just a helper to comment / uncomment test dependencies because
# there is an issue with emacs haskell mode + loading dependencies
#
# @file
# @version 0.1

.PHONY: default

default:
	@echo -n ""

.PHONY: dev, undev

dev:
	@sed -i 's/# - hspec #/- hspec #/' package.yaml
	@sed -i 's/# - QuickCheck #/- QuickCheck #/' package.yaml
	@echo "dev"

undev:
	@sed -i 's/- hspec #/# - hspec #/' package.yaml
	@sed -i 's/- QuickCheck #/# - QuickCheck #/' package.yaml
	@echo "undev"

# end
