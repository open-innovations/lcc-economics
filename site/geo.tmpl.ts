export const layout = 'templates/geo.njk';

export default function* ({ geo }) {
  for (const place of geo) {
    // console.log(place) to check we're getting them OK
    yield {
      title: place,
      url: `/place/${ place.toLowerCase() }/`,
      placeKey: place.toLowerCase()
    }
  }
}