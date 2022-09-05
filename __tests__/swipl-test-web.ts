/**
 * @jest-environment jsdom
 */
import SWIPL from '../dist';

describe('SWIPL', () => {
  it('should be a function', () => {
    expect(SWIPL).toBeInstanceOf(Function);
  });

  // TODO: Work out how to mock browser FS in order to do this
  // it('should return an object containing FS, prolog and locateFile keys', async () => {
    // const mod = await SWIPL({});
    // expect(mod.FS).toBeInstanceOf(Object);
    // expect(mod.prolog).toBeInstanceOf(Object);
    // expect(mod.locateFile).toBeInstanceOf(Function);
    // expect(mod.locateFile('myfile.txt')).toEqual('myfile.txt');
    // expect(mod.locateFile('swipl-web.data')).toEqual('/dist/swipl/swipl-web.data');
  // });
});
