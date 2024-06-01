#!/usr/bin/env bash
# inspired by https://github.com/orhun/git-cliff/blob/main/release.sh
new_version=$(git cliff --bumped-version)
if [ -z "$new_version" ]; then
    echo "No new version found (did git cliff fail?). Exiting."
    exit 1
fi
echo "New version: $new_version"

# update package.yaml version
sed -i'' -e "s/^version: \"[^\"]*\"$/version: \"${new_version#v}\"/" package.yaml || exit 1
# and the cabal file
hpack || exit 1


# Update changelog
git cliff --tag "${new_version}" > CHANGELOG.md
git add -A && git commit -m "chore(release): prepare for ${new_version}"
git show || exit 1

# generate a changelog for the tag message
export GIT_CLIFF_TEMPLATE="\
	{% for group, commits in commits | group_by(attribute=\"group\") %}
	{{ group | upper_first }}\
	{% for commit in commits %}
		- {% if commit.breaking %}(breaking) {% endif %}{{ commit.message | upper_first }} ({{ commit.id | truncate(length=7, end=\"\") }})\
	{% endfor %}
	{% endfor %}"
changelog=$(git cliff --unreleased --strip all)

git tag -s -a "${new_version}" -m "Release ${new_version}" -m "${changelog}"
git tag -v "${new_version}"

echo "Release ready. Run 'git push --follow-tags' to push the changes and tags."
