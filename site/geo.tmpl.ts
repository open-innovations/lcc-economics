export const layout = 'templates/geo.njk';

export default function* ({ geo }) {
  for (const place of geo) {
    console.log(place)
    yield {
      title: place,
      url: `/place/${ place.toLowerCase() }/`,
      placeKey: place.toLowerCase()
    }
  }
}