all :
	dotenv -f .env cabal run github-issues -- phadej tree-diff
