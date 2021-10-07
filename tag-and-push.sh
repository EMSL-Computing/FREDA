#!/bin/bash

TAG_LATEST="false"

while getopts ":n:t:l;" opt; do
  case $opt in
    n) name="$OPTARG"
    ;;
    t) tag="$OPTARG"
    ;;
    l) TAG_LATEST="true"
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    exit 1
    ;;
  esac

  case $OPTARG in
    -*) echo "Option $opt needs a valid argument"
    exit 1
    ;;
  esac
done

printf "Pushing container %s:%s\n" "$name" "$tag"
docker push $name:$tag

if ${TAG_LATEST}; then
    echo "Tagging as latest and pushing..."
    docker tag $name:$tag $name:latest
    docker push $name:latest
fi

