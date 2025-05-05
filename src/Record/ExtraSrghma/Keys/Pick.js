export function pickFn(ks, r) {
  var copy = {};
  for (let i = 0; i < ks.length; i++) {
    copy[ks[i]] = r[ks[i]];
  }
  return copy;
}
